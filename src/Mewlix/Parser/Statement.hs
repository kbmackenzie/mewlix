{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Statement
( root
, Nesting(..)
) where

import Mewlix.Abstract.AST
    ( Params(..)
    , Block(..)
    , Statement(..)
    , MewlixFunction(..)
    , MewlixClass(..)
    )
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Abstract.Module (Module(..))
import Mewlix.Parser.Module (parseModuleKey)
import Mewlix.Parser.Primitive (parseKey)
import Mewlix.Parser.Expression (declaration, exprR, prettyExpr)
import Mewlix.Parser.Utils
    ( Parser
    , keyword
    , longSymbol
    , wordSequence
    , whitespaceLn
    , lexemeLn
    , parens
    , parensList
    , repeatChar
    )
import Mewlix.Keywords.Types
    ( Keyword(..)
    , WordSequence(..)
    )
import qualified Mewlix.Keywords.Constants as Keywords
import qualified Data.List as List
import qualified Text.Megaparsec as Mega
import Control.Monad (void, when)
import Data.Maybe (fromMaybe)

root :: Parser Block
root = do
    let parser :: Parser Block
        parser = Block <$> Mega.many (statement Root)
    Mega.between whitespaceLn Mega.eof parser


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
    , continueKey   nesting
    , breakKey      nesting
    , importKey     nesting
    , forEach       nesting
    , classDef      nesting
    , tryCatch      nesting
    , expression    nesting ]

block :: Nesting -> Parser a -> Parser Block
block nesting stop = do
    let line = do
            Mega.notFollowedBy stop
            statement nesting
    (fmap Block . Mega.many . lexemeLn) line

meowmeow :: Parser ()
meowmeow = Mega.choice
    [ keyword Keywords.end
    , fail "Block is unclosed!" ]


{- Expression -}
----------------------------------------------------------------
expression :: Nesting -> Parser Statement
expression _ = ExpressionStatement <$> exprR


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
    wordSequence Keywords.while
    condition <- parens exprR
    whitespaceLn
    body <- block (max nesting NestedInLoop) meowmeow
    meowmeow
    return (WhileLoop condition body)


{- If Else -}
----------------------------------------------------------------
ifelse :: Nesting -> Parser Statement
ifelse nesting = do
    let localNest = max nesting Nested

    let stopKeys :: Parser ()
        stopKeys = (void . Mega.choice . fmap Mega.try) 
            [ wordSequence  Keywords.elif
            , wordSequence  Keywords.else_
            , keyword       Keywords.end      ]

    let getIf :: WordSequence -> Parser (Block -> Statement)
        getIf key = do
            wordSequence key
            condition <- parens exprR
            whitespaceLn
            body      <- block localNest stopKeys
            return (IfElse condition body)

    let getElse :: Parser Block
        getElse = do
            wordSequence Keywords.else_
            whitespaceLn
            block localNest meowmeow
    
    mainIf      <- getIf Keywords.if_
    elifs       <- Mega.many (getIf Keywords.elif)
    mainElse    <- fromMaybe mempty <$> Mega.optional getElse

    let compose :: (Block -> Statement) -> (Block -> Statement) -> (Block -> Statement)
        compose x acc = x . Block . List.singleton . acc
    let ifs = foldr1 compose (mainIf : elifs)

    meowmeow
    return (ifs mainElse)


{- Functions -}
----------------------------------------------------------------
func :: Parser MewlixFunction
func = do
    longSymbol Keywords.function
    name   <- parseKey
    params <- Params <$> parensList parseKey
    whitespaceLn
    body   <- block Nested meowmeow
    meowmeow
    return (MewlixFunction name params body)

funcDef :: Nesting -> Parser Statement
funcDef _ = FunctionDef <$> func


{- Classes -}
----------------------------------------------------------------
classDef :: Nesting -> Parser Statement
classDef _ = do
    let (clowder, extends) = Keywords.clowder
    keyword clowder
    name        <- parseKey
    parent      <- Mega.optional (keyword extends >> parseKey)
    whitespaceLn
    methods     <- (Mega.many . lexemeLn) func
    constructor <- getConstructor methods
    (return . ClassDef) (MewlixClass name parent constructor methods)

getConstructor :: [MewlixFunction] -> Parser (Maybe MewlixFunction)
getConstructor funcs = do
    let wake = Key (unwrapKeyword Keywords.constructor)
    let constructors = filter ((== wake) . funcName) funcs
    when (length constructors > 1)
        (fail "Class cannot have more than one constructor!")
    let constructor = case constructors of
            []    -> Nothing
            (x:_) -> Just x
    return constructor


{- Return -}
----------------------------------------------------------------
returnKey :: Nesting -> Parser Statement
returnKey _ = do
    keyword Keywords.ret
    Return <$> exprR


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
    wordSequence Keywords.forEach
    iter <- prettyExpr
    repeatChar '!'
    wordSequence Keywords.thenDo
    repeatChar '!'
    key  <- parseKey
    whitespaceLn
    body <- block (max nesting NestedInLoop) meowmeow
    meowmeow
    return (ForEachLoop iter key body)


{- Import -}
----------------------------------------------------------------
importKey :: Nesting -> Parser Statement
importKey nesting = do
    keyword Keywords.takes
    path <- parseModuleKey
    name <- Mega.optional (keyword Keywords.alias >> parseKey)
    when (nesting > Root)
        (fail "Import statements cannot be nested!")
    return $ ImportStatement (Module path name)


{- Watch/Catch -}
----------------------------------------------------------------
tryCatch :: Nesting -> Parser Statement
tryCatch nesting = do
    let localNest = max nesting Nested

    wordSequence Keywords.try
    whitespaceLn
    try_    <- block localNest (wordSequence Keywords.catch)

    wordSequence Keywords.catch
    key_    <- Mega.optional parseKey
    whitespaceLn
    catch_  <- block localNest (keyword Keywords.end)

    meowmeow
    return (TryCatch try_ key_ catch_)
