{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Mewlix.Parser.Statement
( root
, Nesting(..)
) where

import Mewlix.Parser.AST
import Mewlix.Parser.Expr
import Mewlix.Parser.Utils
import Mewlix.Parser.Keywords
import Mewlix.Parser.Prim
import Mewlix.Data.Stack (Stack)
import qualified Mewlix.Data.Stack as Stack
import Data.Text (Text)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Control.Monad (void, when)
import Data.Maybe (fromMaybe)

root :: Parser Block
root = do
    let parser :: Parser Block
        parser = Stack.fromList <$> Mega.many (statement Root)
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
    Stack.fromList <$> (Mega.many . lexemeLn) line

meowmeow :: Parser ()
meowmeow = Mega.choice
    [ keyword meowEnd
    , fail "Block is unclosed!" ]

{- Expression -}
----------------------------------------------------------------
expression :: Nesting -> Parser Statement
expression _ = StmtExpr <$> exprR

{- Declaration -}
----------------------------------------------------------------
declareVar :: Nesting -> Parser Statement
declareVar _ = uncurry StmtDeclaration <$> declaration

{- While -}
----------------------------------------------------------------
whileLoop :: Nesting -> Parser Statement
whileLoop nesting = do
    keyword meowWhile
    condition <- parens exprR
    whitespaceLn
    body <- block (max nesting NestedInLoop) meowmeow
    meowmeow
    return (StmtWhile condition body)

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
            return (StmtIfElse condition body)

    let getElse :: Parser Block
        getElse = do
            keyword meowElse
            whitespaceLn
            block localNest meowmeow
    
    mainIf      <- getIf meowIf
    elifs       <- Mega.many (getIf meowElif)
    mainElse    <- fromMaybe Stack.empty <$> Mega.optional getElse
    let ifs = foldr1 (\x acc -> x . Stack.singleton . acc) (mainIf : elifs)

    meowmeow
    return (ifs mainElse)


{- Functions -}
----------------------------------------------------------------
func :: Parser ParserFunc
func = do
    keyword meowCatface
    name   <- parseName
    params <- parensList parseName
    whitespaceLn
    body   <- block Nested meowmeow
    meowmeow
    return (ParserFunc name params body)

funcDef :: Nesting -> Parser Statement
funcDef _ = StmtFuncDef <$> func


{- Classes -}
----------------------------------------------------------------
classDef :: Nesting -> Parser Statement
classDef _ = do
    keyword meowClass
    name        <- parseName
    extends     <- Mega.optional (keyword meowFrom >> parseName)
    whitespaceLn
    methods     <- (fmap Stack.fromList . Mega.many . lexemeLn) func
    constructor <- getConstructor methods
    (return . StmtClassDef) (ParserClass name extends constructor methods)

getConstructor :: Stack ParserFunc -> Parser (Maybe ParserFunc)
getConstructor funcs = do
    let constructors = Stack.filter ((== meowConstructor) . pFuncName) funcs
    when (Stack.length constructors > 1)
        (fail "Class cannot have more than one constructor!")
    let constructor = if Stack.null constructors
        then Nothing
        else Just (Stack.peek constructors)
    return constructor


{- Return -}
----------------------------------------------------------------
returnKey :: Nesting -> Parser Statement
returnKey _ = do
    keyword meowReturn
    StmtReturn <$> exprR


{- Continue -}
----------------------------------------------------------------
continueKey :: Nesting -> Parser Statement
continueKey nesting = do
    keyword meowContinue
    when (nesting < NestedInLoop)
        (fail "Cannot use loop keyword outside loop!")
    return StmtContinue


{- Break -}
----------------------------------------------------------------
breakKey :: Nesting -> Parser Statement
breakKey nesting = do
    keyword meowBreak
    when (nesting < NestedInLoop)
        (fail "Cannot use loop keyword outside loop!")
    return StmtBreak

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
    return (StmtFor (ini, incr, cond) body)


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
    return (StmtImport path name)


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
            return (`StmtTryCatch` (condition, body))

    mainTry <- getTry
    catches <- Mega.some getCatch
    let catchCompose = foldr1 (\x acc -> acc . Stack.singleton . x) catches

    meowmeow
    return (catchCompose mainTry)
