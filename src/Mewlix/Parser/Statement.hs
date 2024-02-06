{-# LANGUAGE OverloadedStrings #-}

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
import Mewlix.Parser.Expression (declaration, expression)
import Mewlix.Parser.Utils
    ( Parser
    , whitespaceLn
    , lexemeLn
    , repeatChar
    )
import Mewlix.Parser.Keyword (keyword)
import Mewlix.Keywords.Types (SimpleKeyword(..))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Mewlix.Keywords.Constants as Keywords
import qualified Text.Megaparsec as Mega
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.List as List

root :: Parser YarnBall
root = Mega.between whitespaceLn Mega.eof yarnBall

yarnBall :: Parser YarnBall
yarnBall = do
    key <- Mega.optional $ do
        keyword Keywords.yarnball
        parseModuleKey <* whitespaceLn
    body <- Block <$> Mega.many (statement Root)
    return (YarnBall key body)

{- Nesting -}
----------------------------------------------------------------
data Nesting =
      Root
    | Nested
    | NestedInLoop
    deriving (Eq, Ord, Show, Enum, Bounded)

-- The maximum nesting should always propagate.
-- The 'max' function is helpful: Root `max` Nested = Nested


{- Parse statements: -}
----------------------------------------------------------------
statement :: Nesting -> Parser Statement
statement nesting = Mega.choice $ map lexemeLn
    [ whileLoop     nesting
    , ifelse        nesting
    , declareVar    nesting
    , funcDef       nesting
    , returnKey     nesting
    , assert        nesting
    , continueKey   nesting
    , breakKey      nesting
    , importKey     nesting
    , forEach       nesting
    , classDef      nesting
    , tryCatch      nesting
    , expressionStm nesting ]

block :: Nesting -> Maybe (Parser ()) -> Parser Block
block nesting customStop = do
    let stopPoint = fromMaybe (keyword Keywords.end) customStop
    let line = do
            Mega.notFollowedBy stopPoint
            statement nesting
    (fmap Block . Mega.many . lexemeLn) line

meowmeow :: Parser ()
meowmeow = Mega.choice
    [ keyword Keywords.end
    , fail "Block is unclosed!" ]


{- Expression -}
----------------------------------------------------------------
expressionStm :: Nesting -> Parser Statement
expressionStm  _ = ExpressionStatement <$> expression


{- Declaration -}
----------------------------------------------------------------
declareVar :: Nesting -> Parser Statement
declareVar nesting = do
    let bind = if nesting == Root then Binding else LocalBinding
    uncurry bind <$> declaration


{- While -}
----------------------------------------------------------------
whileLoop :: Nesting -> Parser Statement
whileLoop nesting = do
    keyword Keywords.while
    condition <- expression
    whitespaceLn
    body <- block (max nesting NestedInLoop) Nothing
    meowmeow
    return (WhileLoop condition body)


{- If Else -}
----------------------------------------------------------------
ifelse :: Nesting -> Parser Statement
ifelse nesting = do
    let nest = max nesting Nested
    let stopPoint = Mega.choice
            [ keyword Keywords.elif
            , keyword Keywords.else_
            , keyword Keywords.end  ]

    initialConditional <- do
        keyword Keywords.if_
        condition   <- expression
        whitespaceLn
        body        <- block nest (Just stopPoint)
        return (Conditional condition body)

    additonalConditionals <- Mega.many $ do
        keyword Keywords.elif
        condition   <- expression
        whitespaceLn
        body        <- block nest (Just stopPoint)
        return (Conditional condition body)

    elseBlock <- Mega.optional $ do
        keyword Keywords.else_
        whitespaceLn
        block nest Nothing

    meowmeow
    let conditionals = initialConditional :| additonalConditionals
    return (IfElse conditionals elseBlock)


{- Functions -}
----------------------------------------------------------------
func :: Parser MewlixFunction
func = do
    keyword Keywords.function
    name   <- parseKey
    params <- parseParams
    whitespaceLn
    body   <- block Nested Nothing
    meowmeow
    return (MewlixFunction name params body)

funcDef :: Nesting -> Parser Statement
funcDef _ = FunctionDef <$> func


{- Classes -}
----------------------------------------------------------------
type Constructor = MewlixFunction
type Methods     = [MewlixFunction]

classDef :: Nesting -> Parser Statement
classDef _ = do
    keyword Keywords.clowder
    name    <- parseKey
    parent  <- Mega.optional (keyword Keywords.extends >> parseKey)
    whitespaceLn
    (constructor, methods) <- (Mega.many . lexemeLn) func >>= sortConstructor
    meowmeow

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
----------------------------------------------------------------
returnKey :: Nesting -> Parser Statement
returnKey _ = do
    keyword Keywords.ret
    Return <$> expression

{- Assert -}
----------------------------------------------------------------
assert :: Nesting -> Parser Statement
assert _ = do
    keyword Keywords.assert
    Assert <$> expression <*> Mega.getSourcePos

{- Continue -}
----------------------------------------------------------------
continueKey :: Nesting -> Parser Statement
continueKey nesting = do
    keyword Keywords.catnap
    when (nesting < NestedInLoop)
        (fail "Cannot use loop keyword outside loop!")
    return Continue


{- Break -}
----------------------------------------------------------------
breakKey :: Nesting -> Parser Statement
breakKey nesting = do
    keyword Keywords.break
    when (nesting < NestedInLoop)
        (fail "Cannot use loop keyword outside loop!")
    return Break

{- For Loop -}
----------------------------------------------------------------
forEach :: Nesting -> Parser Statement
forEach nesting = do
    keyword Keywords.forEach
    iter <- expression
    repeatChar '!'
    keyword Keywords.thenDo
    key  <- parseKey
    repeatChar '!'
    whitespaceLn
    body <- block (max nesting NestedInLoop) Nothing
    meowmeow
    return (ForEachLoop iter key body)


{- Import -}
----------------------------------------------------------------
importKey :: Nesting -> Parser Statement
importKey _ = do
    keyword Keywords.takes
    path <- parseModuleKey
    name <- Mega.optional (keyword Keywords.alias >> parseKey)
    return $ ImportStatement (ModuleData path name)


{- Watch/Catch -}
----------------------------------------------------------------
tryCatch :: Nesting -> Parser Statement
tryCatch nesting = do
    let localNest = max nesting Nested

    keyword Keywords.try
    whitespaceLn
    try_    <- block localNest (Just $ keyword Keywords.catch)

    keyword Keywords.catch
    key_    <- Mega.optional parseKey
    whitespaceLn
    catch_  <- block localNest Nothing

    meowmeow
    return (TryCatch try_ key_ catch_)
