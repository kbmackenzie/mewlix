{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Statement
( root
, Nesting(..)
) where

import Mewlix.Abstract.AST
import Mewlix.Parser.Utils
import Mewlix.Parser.Keywords
import Mewlix.Parser.Primitive
import Mewlix.Parser.Expression
import Data.Text (Text)
import qualified Data.List as List
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Control.Monad (void, when)
import Data.Maybe (fromMaybe)

root :: Parser Block
root = do
    let parser :: Parser Block
        parser = Mega.many (statement Root)
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
    , classDef      nesting
    , forLoop       nesting
    , tryCatch      nesting
    , expression    nesting ]

block :: Nesting -> Parser a -> Parser Block
block nesting stop = do
    let line = do
            Mega.notFollowedBy stop
            statement nesting
    (Mega.many . lexemeLn) line

meowmeow :: Parser ()
meowmeow = Mega.choice
    [ keyword meowEnd
    , fail "Block is unclosed!" ]


{- Expression -}
----------------------------------------------------------------
expression :: Nesting -> Parser Statement
expression _ = ExpressionStatement <$> exprR


{- Declaration -}
----------------------------------------------------------------
declareVar :: Nesting -> Parser Statement
declareVar _ = uncurry Declaration <$> declaration


{- While -}
----------------------------------------------------------------
whileLoop :: Nesting -> Parser Statement
whileLoop nesting = do
    keyword meowWhile
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
        stopKeys = (void . Mega.choice . fmap MChar.string) 
            [ meowElif
            , meowElse
            , meowEnd ]

    let getIf :: Text -> Parser (Block -> Statement)
        getIf key = do
            keyword key
            condition <- parens exprR
            whitespaceLn
            body      <- block localNest stopKeys
            return (IfElse condition body)

    let getElse :: Parser Block
        getElse = do
            keyword meowElse
            whitespaceLn
            block localNest meowmeow
    
    mainIf      <- getIf meowIf
    elifs       <- Mega.many (getIf meowElif)
    mainElse    <- fromMaybe [] <$> Mega.optional getElse
    let ifs = foldr1 (\x acc -> x . List.singleton . acc) (mainIf : elifs)

    meowmeow
    return (ifs mainElse)


{- Functions -}
----------------------------------------------------------------
func :: Parser MewlixFunction
func = do
    keyword meowCatface
    name   <- parseName
    params <- parensList parseName
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
    keyword meowClass
    name        <- parseName
    extends     <- Mega.optional (keyword meowFrom >> parseName)
    whitespaceLn
    methods     <- (Mega.many . lexemeLn) func
    constructor <- getConstructor methods
    (return . ClassDef) (MewlixClass name extends constructor methods)

getConstructor :: [MewlixFunction] -> Parser (Maybe MewlixFunction)
getConstructor funcs = do
    let constructors = filter ((== meowConstructor) . pFuncName) funcs
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
    keyword meowReturn
    Return <$> exprR


{- Continue -}
----------------------------------------------------------------
continueKey :: Nesting -> Parser Statement
continueKey nesting = do
    keyword meowContinue
    when (nesting < NestedInLoop)
        (fail "Cannot use loop keyword outside loop!")
    return Continue


{- Break -}
----------------------------------------------------------------
breakKey :: Nesting -> Parser Statement
breakKey nesting = do
    keyword meowBreak
    when (nesting < NestedInLoop)
        (fail "Cannot use loop keyword outside loop!")
    return Break

{- For Loop -}
----------------------------------------------------------------
forLoop :: Nesting -> Parser Statement
forLoop nesting = do
    let (start, middle, end) = meowFor
    keyword start
    ini  <- parens liftedExpr
    keyword middle
    incr <- parens exprR
    keyword end
    cond <- parens exprR
    whitespaceLn
    body <- block (max nesting NestedInLoop) meowmeow
    meowmeow
    return (ForLoop (ini, incr, cond) body)


{- Import -}
----------------------------------------------------------------
importKey :: Nesting -> Parser Statement
importKey nesting = do
    let (start, end) = meowTakes
    keyword start
    path <- parseString
    name <- Mega.optional (keyword end >> parseName)
    when (nesting > Root)
        (fail "Import statements cannot be nested!")
    return (ImportStatement path name)


{- Watch/Catch -}
----------------------------------------------------------------
tryCatch :: Nesting -> Parser Statement
tryCatch nesting = do
    let localNest = max nesting Nested

    let stopKeys :: Parser ()
        stopKeys = (void . Mega.choice . fmap MChar.string) [ meowCatch , meowEnd ]

    let getTry :: Parser Block
        getTry = do
            keyword meowTry
            whitespaceLn
            block localNest stopKeys

    let getCatch :: Parser (Block -> Statement)
        getCatch = do
            keyword meowCatch
            condition <- Mega.optional (parens exprR)
            whitespaceLn
            body      <- block localNest stopKeys
            return (`TryCatch` (condition, body))

    mainTry <- getTry
    catches <- Mega.some getCatch
    let catchCompose = foldr1 (\x acc -> acc . List.singleton . x) catches

    meowmeow
    return (catchCompose mainTry)
