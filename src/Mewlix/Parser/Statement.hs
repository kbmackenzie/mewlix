{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Parser.Statement
( root
) where

import Mewlix.Parser.Type
    ( Parser
    , asks
    , local
    , nested
    , addNesting
    , defineNesting
    , Nesting
    , NestingFlag(..)
    )
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
import Mewlix.Parser.Expression (expression, arguments)
import Mewlix.Parser.Utils
    ( linebreak
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
import Control.Monad (void, unless)
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
    body <- Block <$> Mega.many statement
    return (YarnBall key body)

{- Parse statements: -}
------------------------------------------------------------------
statement :: Parser Statement
statement = choose
    [ whileLoop
    , ifelse
    , declaration
    , funcDef
    , returnKey
    , classDef
    , superCall
    , assert
    , continueKey
    , breakKey
    , importKey
    , importList
    , forEach
    , throwException
    , tryCatch
    , expressionStm ]
    <?> "statement"
    where choose = Mega.choice . map multiline

block :: Maybe (Parser ()) -> Parser Block
block customStop = do
    let stopPoint = fromMaybe (keyword Keywords.end) customStop
    fmap Block . Mega.many $ do
        Mega.notFollowedBy stopPoint
        statement

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

declaration :: Parser Statement
declaration = do
    let rvalue :: Parser Expression
        rvalue = Mega.choice
            [ symbol '=' >> (expression <* linebreak)
            , return $ PrimitiveExpr MewlixNil        ]
    keyword Keywords.local
    binding <*> rvalue

{- While -}
------------------------------------------------------------------
whileLoop :: Parser Statement
whileLoop = do
    condition <- open $ do
        keyword Keywords.while
        expression
    body <- local (addNesting InLoop) $ block Nothing
    close
    return (WhileLoop condition body)

{- If Else -}
------------------------------------------------------------------
ifelse :: Parser Statement
ifelse = do
    let stopPoints = Mega.choice
            [ keyword Keywords.elif
            , keyword Keywords.else_
            , keyword Keywords.end   ]
    lookIf <- do
        condition <- open $ do
            keyword Keywords.if_
            expression
        body <- block (Just stopPoints)
        return (Conditional condition body)
    orIfs <- Mega.many $ do
        condition <- open $ do
            keyword Keywords.elif
            expression
        body <- block (Just stopPoints)
        return (Conditional condition body)
    elseJust <- Mega.optional $ do
        open (keyword Keywords.else_)
        block Nothing
    close
    let conditionals = lookIf :| orIfs
    return (IfElse conditionals elseJust)

{- Functions -}
------------------------------------------------------------------
func :: (Nesting -> Nesting) -> Parser MewlixFunction
func nesting = do
    (name, params) <- open $ do
        keyword Keywords.function
        name   <- parseKey
        params <- parseParams
        return (name, params)
    body   <- local nesting $ block Nothing
    close
    return (MewlixFunction name params body)

funcDef :: Parser Statement
funcDef = FunctionDef <$> func nesting
    where nesting = defineNesting [InFunction]

{- Return -}
------------------------------------------------------------------
returnKey :: Parser Statement
returnKey = do
    keyword Keywords.ret
    inFunction <- asks (nested InFunction)
    unless inFunction
        (fail "Cannot use function keyword outside function!")
    Return <$> expression <* linebreak

{- Classes -}
------------------------------------------------------------------
type Constructor = MewlixFunction
type Methods     = [MewlixFunction]

classDef :: Parser Statement
classDef = do
    (name, parent) <- open $ do
        keyword Keywords.clowder
        name    <- parseKey
        parent  <- Mega.optional $ do
            keyword Keywords.extends
            parseKey
        return (name, parent)
    (constructor, methods) <- do
        let methodNesting = defineNesting [InFunction, InClass]
        methods <- (Mega.many . multiline) (func methodNesting)
        findConstructor methods
    close

    let patchedConstructor = fmap patchConstructor constructor
    let patchedMethods = maybe methods (: methods) patchedConstructor

    (return . ClassDef) MewlixClass
        { className         = name
        , classExtends      = parent
        , classMethods      = patchedMethods
        , classConstructor  = patchedConstructor }

findConstructor :: Methods -> Parser (Maybe Constructor, Methods)
findConstructor methods = do
    let wake = Key (unwrapKeyword Keywords.constructor)
    let predicate = (== wake) . funcName

    case List.partition predicate methods of
        ([] , xs) -> return (Nothing, xs)
        ([x], xs) -> return (Just x, xs)
        _         -> fail "Clowder cannot have more than one constructor!"

patchConstructor :: Constructor -> Constructor
patchConstructor constructor = do
    let returnHome = Return (PrimitiveExpr MewlixHome)
    let patch = (<> Block [returnHome])
    constructor { funcBody = patch (funcBody constructor) }

{- Super -}
------------------------------------------------------------------
superCall :: Parser Statement
superCall = do
    args <- do
        keyword Keywords.superCall
        fromMaybe mempty <$> Mega.optional arguments
    inClass <- asks (nested InClass)
    unless inClass
        (fail "Cannot call parent constructor outside clowder!")
    SuperCall args <$ linebreak

{- Assert -}
------------------------------------------------------------------
assert :: Parser Statement
assert = do
    keyword Keywords.assert
    Assert <$> (expression <* linebreak) <*> Mega.getSourcePos

{- Continue -}
------------------------------------------------------------------
continueKey :: Parser Statement
continueKey = do
    keyword Keywords.catnap
    inLoop <- asks (nested InLoop)
    unless inLoop
        (fail "Cannot use loop keyword outside loop!")
    Continue <$ linebreak

{- Break -}
------------------------------------------------------------------
breakKey :: Parser Statement
breakKey = do
    keyword Keywords.break
    inLoop <- asks (nested InLoop)
    unless inLoop
        (fail "Cannot use loop keyword outside loop!")
    Break <$ linebreak

{- For Loop -}
------------------------------------------------------------------
forEach :: Parser Statement
forEach = do
    (key, iter) <- open $ do
        keyword Keywords.forEach
        key  <- parseKey
        keyword Keywords.forEachOf
        iter <- expression
        void . Mega.optional $ repeatChar '!'
        return (key, iter)
    body <- local (addNesting InLoop) $ block Nothing
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

{- Errors -}
------------------------------------------------------------------
throwException :: Parser Statement
throwException = do
    keyword Keywords.throw
    pos  <- Mega.getSourcePos
    expr <- expression <* linebreak
    return (ThrowError expr pos)

tryCatch :: Parser Statement
tryCatch = do
    open (keyword Keywords.try)
    tryBlock <- block (Just $ keyword Keywords.catch)
    key <- open $ do
        keyword Keywords.catch
        Mega.optional parseKey
    catchBlock <- local (addNesting InTryCatch) $ block Nothing
    close
    return (TryCatch tryBlock key catchBlock)
