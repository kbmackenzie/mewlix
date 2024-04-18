{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Parser.Statement
( root
, Nesting(..)
) where

import Mewlix.Abstract.AST
    ( Block(..)
    , Primitive(..)
    , Expression(..)
    , Statement(..)
    , MewlixFunction(..)
    , MewlixClass(..)
    , Conditional(..)
    , YarnBall(..),
    )
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Abstract.Module (ModuleData(..))
import Mewlix.Parser.Module (parseModuleKey)
import Mewlix.Parser.Primitive (parseKey, parseParams)
import Mewlix.Parser.Expression (expression)
import Mewlix.Parser.Utils
    ( Parser
    , linebreak
    , skipLines
    , multiline
    , repeatChar
    , symbol
    )
import Mewlix.Parser.Keyword (keyword)
import Mewlix.Keywords.Types (SimpleKeyword(..))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Mewlix.Keywords.LanguageKeywords as Keywords
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec as Mega
import Control.Monad (when, void)
import Data.Maybe (fromMaybe)
import qualified Data.List as List
import Data.Functor ((<&>))

root :: Parser YarnBall
root = Mega.between skipLines Mega.eof yarnBall

yarnBall :: Parser YarnBall
yarnBall = do
    key  <- (<?> "yarn ball") . Mega.optional $ do
        keyword Keywords.yarnball <|> keyword Keywords.yarnball'
        parseModuleKey <* linebreak
    body <- Block <$> Mega.many (statement Root)
    return (YarnBall key body)

{- Nesting -}
------------------------------------------------------------------
data Nesting =
      Root
    | Nested
    | NestedInLoop
    deriving (Eq, Ord, Show, Enum, Bounded)

-- The maximum nesting should always propagate.
-- The 'max' function is helpful: Root `max` Nested = Nested

{- Parse statements: -}
------------------------------------------------------------------
statement :: Nesting -> Parser Statement
statement nesting = choose
    [ whileLoop     nesting
    , ifelse        nesting
    , declaration   nesting
    , funcDef
    , returnKey
    , assert
    , continueKey   nesting
    , breakKey      nesting
    , importKey
    , importList
    , forEach       nesting
    , classDef
    , tryCatch      nesting
    , expressionStm         ]
    <?> "statement"
    where choose = Mega.choice . map multiline

block :: Nesting -> Maybe (Parser ()) -> Parser Block
block nesting customStop = do
    let stopPoint = fromMaybe (keyword Keywords.end) customStop
    fmap Block . Mega.many $ do
        Mega.notFollowedBy stopPoint
        statement nesting

open :: Parser a -> Parser a
open = (<* linebreak)

close :: Parser ()
close = Mega.choice
    [ keyword Keywords.end >> linebreak
    , fail "possibly unclosed block"    ]

{- Expression -}
------------------------------------------------------------------
expressionStm :: Parser Statement
expressionStm = ExpressionStatement <$> (expression <* linebreak)

{- Declaration -}
------------------------------------------------------------------
binding :: Parser (Expression -> Statement)
binding = do
    key <- parseKey
    Mega.optional (repeatChar '!') <&> \case
        Nothing -> Variable key
        _       -> Constant key

declaration :: Nesting -> Parser Statement
declaration _ = do
    let rvalue :: Parser Expression
        rvalue = Mega.choice
            [ symbol '=' >> expression <* linebreak
            , return $ PrimitiveExpr MewlixNil ]
    keyword Keywords.local
    binding <*> rvalue

{- While -}
------------------------------------------------------------------
whileLoop :: Nesting -> Parser Statement
whileLoop nesting = do
    condition <- open $ do
        keyword Keywords.while
        expression
    body <- block (max nesting NestedInLoop) Nothing
    close
    return (WhileLoop condition body)

{- If Else -}
------------------------------------------------------------------
ifelse :: Nesting -> Parser Statement
ifelse nesting = do
    let nest = max nesting Nested
    let stopPoint = Mega.choice
            [ keyword Keywords.elif
            , keyword Keywords.else_
            , keyword Keywords.end  ]

    initialConditional <- do
        condition <- open $ do
            keyword Keywords.if_
            expression
        body      <- block nest (Just stopPoint)
        return (Conditional condition body)

    additonalConditionals <- Mega.many $ do
        condition <- open $ do
            keyword Keywords.elif
            expression
        body      <- block nest (Just stopPoint)
        return (Conditional condition body)

    elseBlock <- Mega.optional $ do
        open (keyword Keywords.else_)
        block nest Nothing

    close
    let conditionals = initialConditional :| additonalConditionals
    return (IfElse conditionals elseBlock)

{- Functions -}
------------------------------------------------------------------
func :: Parser MewlixFunction
func = do
    (name, params) <- open $ do
        keyword Keywords.function
        name   <- parseKey
        params <- parseParams
        return (name, params)
    body   <- block Nested Nothing
    close
    return (MewlixFunction name params body)

funcDef :: Parser Statement
funcDef = FunctionDef <$> func

{- Classes -}
------------------------------------------------------------------
type Constructor = MewlixFunction
type Methods     = [MewlixFunction]

classDef :: Parser Statement
classDef = do
    (name, parent) <- open $ do
        keyword Keywords.clowder
        name    <- parseKey
        parent  <- Mega.optional (keyword Keywords.extends >> parseKey)
        return (name, parent)
    (constructor, methods) <- (Mega.many . multiline) func >>= sortConstructor
    close

    let patchedConstructor = fmap patchConstructor constructor
    let patchedMethods = maybe methods (: methods) patchedConstructor

    (return . ClassDef) MewlixClass
        { className         = name
        , classExtends      = parent
        , classMethods      = patchedMethods
        , classConstructor  = patchedConstructor }

sortConstructor :: Methods -> Parser (Maybe Constructor, Methods) 
sortConstructor methods = do
    let wake = Key (unwrapKeyword Keywords.constructor)
    let predicate = (== wake) . funcName
    case List.partition predicate methods of
        ([] , xs)   -> return (Nothing, xs)
        ([x], xs)   -> return (Just x, xs)
        _           -> fail "Clowder cannot have more than one constructor!"

patchConstructor :: Constructor -> Constructor
patchConstructor constructor = do
    let returnHome = Return (PrimitiveExpr MewlixHome)
    let patch = (<> Block [returnHome])
    constructor { funcBody = patch (funcBody constructor) }

{- Return -}
------------------------------------------------------------------
returnKey :: Parser Statement
returnKey = do
    keyword Keywords.ret
    Return <$> expression <* linebreak

{- Assert -}
------------------------------------------------------------------
assert :: Parser Statement
assert = do
    keyword Keywords.assert
    Assert <$> (expression <* linebreak) <*> Mega.getSourcePos

{- Continue -}
------------------------------------------------------------------
continueKey :: Nesting -> Parser Statement
continueKey nesting = do
    keyword Keywords.catnap >> linebreak
    when (nesting < NestedInLoop)
        (fail "Cannot use loop keyword outside loop!")
    return Continue

{- Break -}
------------------------------------------------------------------
breakKey :: Nesting -> Parser Statement
breakKey nesting = do
    keyword Keywords.break >> linebreak
    when (nesting < NestedInLoop)
        (fail "Cannot use loop keyword outside loop!")
    return Break

{- For Loop -}
------------------------------------------------------------------
forEach :: Nesting -> Parser Statement
forEach nesting = do
    (key, iter) <- open $ do
        keyword Keywords.forEach
        key  <- parseKey
        keyword Keywords.forEachOf
        iter <- expression
        void . Mega.optional $ repeatChar '!'
        return (key, iter)
    body <- block (max nesting NestedInLoop) Nothing
    close
    return (ForEachLoop iter key body)

{- Import -}
------------------------------------------------------------------
importKey :: Parser Statement
importKey = do
    keyword Keywords.takes
    path <- parseModuleKey
    name <- Mega.optional (keyword Keywords.alias >> parseKey)
    linebreak
    return $ ImportModule (ModuleData path name)

importList :: Parser Statement
importList = do
    keyword Keywords.from
    path <- parseModuleKey
    keyword Keywords.takes
    keys <- Mega.sepBy1 parseKey (symbol ',')
    linebreak
    return $ ImportList (ModuleData path Nothing) keys

{- Watch/Catch -}
------------------------------------------------------------------
tryCatch :: Nesting -> Parser Statement
tryCatch nesting = do
    let localNest = max nesting Nested

    open (keyword Keywords.try)
    try_    <- block localNest (Just $ keyword Keywords.catch)

    key_    <- open $ do
        keyword Keywords.catch
        Mega.optional parseKey
    catch_  <- block localNest Nothing

    close
    return (TryCatch try_ key_ catch_)
