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
    , Params(..)
    , Primitive(..)
    , Expression(..)
    , Statement(..)
    , MewlixFunction(..)
    , MewlixClass(..)
    , MewlixEnum(..)
    , Conditional(..)
    , YarnBall(..),
    )
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Abstract.Module (ModuleData(..))
import Mewlix.Parser.Module (parseModuleKey)
import Mewlix.Parser.Primitive (parseKey, parseParams)
import Mewlix.Parser.Expression (expression, lvalue, arguments)
import Mewlix.Parser.Utils (linebreak, skipLines, multiline, symbol, brackets, repeatChar, lexeme)
import Mewlix.Parser.Keyword (keyword)
import Mewlix.Keywords.Types (SimpleKeyword(..))
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Mewlix.Keywords.Constants as Keywords
import Text.Megaparsec ((<|>), (<?>), label)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Control.Monad (void, unless)
import Data.Maybe (fromMaybe)
import qualified Data.List as List
import Data.Functor ((<&>))
import Prelude hiding (lines, break)
import Data.Bifunctor (first)

root :: Parser YarnBall
root = Mega.between skipLines Mega.eof yarnball

yarnball :: Parser YarnBall
yarnball = do
    key <- label "yarn ball" . Mega.optional $ do
        Mega.choice
            [ keyword Keywords.yarnball
            , keyword Keywords.yarnball' ]
        parseModuleKey <* linebreak
    body <- (fmap Block . Mega.many) statement
    return $ YarnBall key body

{- Parsing Statements -}
------------------------------------------------------------------
open :: Parser a -> Parser a
open = (<* linebreak)

close :: Parser ()
close = keyword Keywords.end >> linebreak

block :: Parser Block
block = do
    let lines :: Parser [Statement]
        lines = ([] <$ close) <|> do
            line <- statement <|> fail "possibly unclosed block"
            (line :) <$> lines
    Block <$> lines

{- Expressions -}
------------------------------------------------------------------
expressionStatement :: Parser Statement
expressionStatement = ExpressionStatement <$> (expression <* linebreak)

{- Bindings -}
------------------------------------------------------------------
binding :: Parser (Expression -> Statement)
binding = do
    key <- parseKey
    Mega.optional (repeatChar '!') <&> \case
        Nothing -> Variable key
        _       -> Constant key

declaration :: Parser Statement
declaration = do
    let value :: Parser Expression
        value = Mega.choice
            [ symbol '=' >> (expression <* linebreak)
            , return (PrimitiveExpr MewlixNil) ]
    keyword Keywords.local
    binding <*> value

assignment :: Parser Statement
assignment = do
    key <- Mega.try $ do
        key <- lvalue
        lexeme $ do
            let equal = MChar.char '='
            void equal
            Mega.notFollowedBy equal
        return key
    Assignment key <$> expression

{- Loops -}
------------------------------------------------------------------
whileLoop :: Parser Statement
whileLoop = do
    condition <- open $ do
        keyword Keywords.while
        expression
    body <- local (addNesting InLoop) block
    return $ WhileLoop condition body

continue :: Parser Statement
continue = do
    keyword Keywords.catnap
    inLoop <- asks (nested InLoop)
    unless inLoop
        (fail "Cannot use loop keyword outside loop!")
    Continue <$ linebreak

break :: Parser Statement
break = do
    keyword Keywords.break
    inLoop <- asks (nested InLoop)
    unless inLoop
        (fail "Cannot use loop keyword outside loop!")
    Break <$ linebreak

forEach :: Parser Statement
forEach = do
    (key, iterator) <- open $ do
        keyword Keywords.forEach
        key <- parseKey
        keyword Keywords.forEachOf
        iterator <- expression
        (void . Mega.optional . repeatChar) '!'
        return (key, iterator)
    body <- local (addNesting InLoop) block
    return $ ForEachLoop iterator key body

{- Control Flow -}
------------------------------------------------------------------
type IfElseBuilder = Conditional -> (NonEmpty Conditional, Maybe Block)

ifElse :: Parser Statement
ifElse = do
    let parseElse :: Parser IfElseBuilder
        parseElse = do
            open (keyword Keywords.else_)
            body <- block
            return $ \conditional -> (conditional :| [], Just body)

    let parseEnd :: Parser IfElseBuilder
        parseEnd = do
            close
            return $ \conditional -> (conditional :| [], Nothing)

    let parseElif :: Parser IfElseBuilder
        parseElif = do
            condition <- open $ do
                keyword Keywords.elif
                expression

            let next :: [Statement] -> Parser IfElseBuilder
                next lines = do
                    builder <- parseEnd <|> parseElse <|> parseElif
                    let self = Conditional condition . Block  . reverse $ lines
                    return $ \conditional -> first (conditional <|) (builder self)

            let parse :: [Statement] -> Parser IfElseBuilder
                parse lines = next lines <|> do
                    line <- statement
                    parse (line : lines)
            parse []

    let parseIf :: Parser Statement
        parseIf = do
            condition <- open $ do
                keyword Keywords.if_
                expression

            let next :: [Statement] -> Parser Statement
                next lines = do
                    builder <- parseEnd <|> parseElse <|> parseElif
                    let self = Conditional condition . Block . reverse $ lines
                    let (branches, elseblock) = builder self
                    return $ IfElse branches elseblock

            let parse :: [Statement] -> Parser Statement
                parse lines = next lines <|> do
                    line <- statement
                    parse (line : lines)
            parse []
    parseIf

{- Functions -}
------------------------------------------------------------------
functionLike :: Parser () -> Parser key -> (Nesting -> Nesting) -> Parser (key, Params, Block)
functionLike opener parseKey_ nesting = do
    opener
    (key, params) <- open $ do
        key    <- parseKey_
        params <- parseParams
        return (key, params)
    body <- local nesting block
    return (key, params, body)

function :: (Nesting -> Nesting) -> Parser MewlixFunction
function nesting = do
    let opener = Mega.try $ do
            keyword Keywords.function
            Mega.notFollowedBy (symbol '[')
    (key, params, body) <- functionLike opener parseKey nesting
    return $ MewlixFunction
        { funcName   = key
        , funcBody   = body
        , funcParams = params }

functionDef :: Parser Statement
functionDef = FunctionDef <$> function nesting
    where nesting = defineNesting [InFunction]

functionAssign :: Parser Statement
functionAssign = do
    let key     = brackets lvalue
    let opener  = keyword Keywords.function
    let nesting = defineNesting [InFunction]
    (expr, params, body) <- functionLike opener key nesting
    return . FunctionAssignment expr $ MewlixFunction
        { funcName   = mempty
        , funcBody   = body
        , funcParams = params }

{- Return -}
------------------------------------------------------------------
returnStatement :: Parser Statement
returnStatement = do
    keyword Keywords.ret
    inFunction <- asks (nested InFunction)
    unless inFunction
        (fail "Cannot use function keyword outside function!")
    Return <$> expression <* linebreak

earlyReturn :: Parser Statement
earlyReturn = do
    keyword Keywords.earlyRet
    inFunction <- asks (nested InFunction)
    unless inFunction
        (fail "Cannot use function keyword outside function!")
    Return (PrimitiveExpr MewlixNil) <$ linebreak

{- Classes -}
------------------------------------------------------------------
type Constructor = MewlixFunction
type Methods     = [MewlixFunction]

classDef :: Parser Statement
classDef = do
    (name, parent) <- open $ do
        keyword Keywords.clowder
        name   <- parseKey
        parent <- Mega.optional $ do
            keyword Keywords.extends
            parseKey
        return (name, parent)
    (constructor, methods) <- do
        let methodNesting = defineNesting [InFunction, InClass]
        methods <- Mega.many . multiline $ function methodNesting
        findConstructor methods
    close
    (return . ClassDef) MewlixClass
        { className         = name
        , classExtends      = parent
        , classMethods      = methods 
        , classConstructor  = constructor }

findConstructor :: Methods -> Parser (Maybe Constructor, Methods)
findConstructor methods = do
    let wake = Key (unwrapKeyword Keywords.constructor)
    let predicate = (== wake) . funcName

    case List.partition predicate methods of
        ([] , xs) -> return (Nothing, xs)
        ([x], xs) -> return (Just x, xs)
        _         -> fail "Clowder cannot have more than one constructor!"

superCall :: Parser Statement
superCall = do
    args <- do
        keyword Keywords.superCall
        fromMaybe mempty <$> Mega.optional arguments
    inClass <- asks (nested InClass)
    unless inClass
        (fail "Cannot call parent constructor outside clowder!")
    SuperCall args <$ linebreak

{- Miscellaneous -}
------------------------------------------------------------------
enumDef :: Parser Statement
enumDef = do
    name <- open $ do
        keyword Keywords.catTree
        parseKey
    keys <- Mega.some (parseKey <* linebreak)
    close
    return . EnumDef $ MewlixEnum
        { enumName = name
        , enumKeys = keys }

assert :: Parser Statement
assert = do
    keyword Keywords.assert
    pos  <- Mega.getSourcePos
    expr <- expression <* linebreak
    return $ Assert expr pos

importAll :: Parser Statement
importAll = do
    keyword Keywords.takes
    key   <- parseModuleKey
    alias <- Mega.optional $ keyword Keywords.alias >> parseKey
    linebreak
    return . ImportModule $ ModuleData key alias

importSome :: Parser Statement
importSome = do
    keyword Keywords.from
    key  <- parseModuleKey
    keyword Keywords.takes
    list <- Mega.sepBy1 parseKey (symbol ',')
    linebreak
    return $ ImportList (ModuleData key Nothing) list

{- Error Handling -}
------------------------------------------------------------------
throwStatement :: Parser Statement
throwStatement = do
    keyword Keywords.throw
    pos  <- Mega.getSourcePos
    expr <- expression <* linebreak
    return $ ThrowError expr pos

rethrow :: Parser Statement
rethrow = do
    keyword Keywords.rethrow
    isCatch <- asks (nested InCatch)
    unless isCatch
        (fail "Cannot use this statement outside error handling statement!")
    Rethrow <$ linebreak

tryCatch :: Parser Statement
tryCatch = do
    let parseCatch :: Parser (Block -> Statement)
        parseCatch = do
            key <- open $ do
                keyword Keywords.catch
                Mega.optional parseKey
            catchblock <- local (addNesting InCatch) block
            return $ \tryblock -> TryCatch tryblock key catchblock

    let parseTry :: Parser Statement
        parseTry = do
            open (keyword Keywords.try)

            let next :: [Statement] -> Parser Statement
                next lines = do
                    builder <- parseCatch
                    let self = Block . reverse $ lines
                    return $ builder self

            let parse :: [Statement] -> Parser Statement
                parse lines = next lines <|> do
                    line <- statement
                    parse (line : lines)
            parse []
    parseTry

{- All Statements -}
------------------------------------------------------------------
statement :: Parser Statement
statement = choose
    [ declaration
    , functionDef
    , functionAssign
    , returnStatement
    , earlyReturn
    , classDef
    , superCall
    , enumDef
    , whileLoop
    , ifElse
    , forEach
    , assert
    , continue
    , break
    , importAll
    , importSome
    , throwStatement
    , tryCatch
    , rethrow
    , assignment
    , expressionStatement ]
    <?> "statement"
    where choose = Mega.choice . map multiline
