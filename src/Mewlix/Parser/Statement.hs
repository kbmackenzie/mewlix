{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Mewlix.Parser.Statement
( root
, ifelse
, whileLoop
, Nesting(..)
) where

import Mewlix.Parser.AST
import Mewlix.Parser.Expr
import Mewlix.Parser.Utils
import Mewlix.Parser.Keywords
import Mewlix.Parser.Prim
import qualified Mewlix.Data.Stack as Stack
import Text.Megaparsec ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
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
statement nesting = Mega.choice $ map lexemeMultiline
    [ whileLoop     nesting
    , ifelse        nesting
    , declareVar    nesting
    , func          nesting
    , returnKey     nesting
    , continueKey   nesting
    , breakKey      nesting
    , importKey     nesting
    , forLoop       nesting
    , tryCatch      nesting
    , expression    nesting ]

block :: Nesting -> Parser a -> Parser Block
block nesting stop = do
    let line = do
            Mega.notFollowedBy stop
            statement nesting
    Stack.fromList <$> (Mega.many . lexemeMultiline) line

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
    let nest = max nesting Nested

    let stop :: Parser ()
        stop = (void . Mega.choice . fmap MChar.string) 
            [ meowElif
            , meowElse
            , meowEnd ]

    let if_ :: Text -> Parser (Block -> Statement)
        if_ key = do
            keyword key
            condition <- parens exprR
            whitespaceLn
            body      <- block nest stop
            return (StmtIfElse condition body)

    let else_ :: Parser Block
        else_ = do
            keyword meowElse
            whitespaceLn
            block nest meowmeow
    
    mainIf      <- if_ meowIf
    elifs       <- Mega.many (if_ meowElif)
    mainElse    <- fromMaybe Stack.empty <$> Mega.optional else_
    let ifs = foldr1 (\x acc -> x . Stack.singleton . acc) (mainIf : elifs)

    meowmeow
    return (ifs mainElse)


{- Functions -}
----------------------------------------------------------------
func :: Nesting -> Parser Statement
func _ = do
    keyword meowCatface
    name   <- parseName
    params <- parensList parseName
    whitespaceLn
    body   <- block Nested meowmeow
    meowmeow
    return (StmtFuncDef name params body)


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
    let nest = max nesting Nested

    let stop :: Parser ()
        stop = (void . Mega.choice . fmap MChar.string) [ meowCatch , meowEnd ]

    let try_ :: Parser Block
        try_ = do
            keyword meowTry
            whitespaceLn
            block nest stop

    let catch_ :: Parser (Block -> Statement)
        catch_ = do
            keyword meowCatch
            condition <- Mega.optional (parens exprR)
            whitespaceLn
            body      <- block nest stop
            return (`StmtTryCatch` (condition, body))

    mainTry <- try_
    catches <- Mega.some catch_
    let catchCompose = foldr1 (\x acc -> acc . Stack.singleton . x) catches

    meowmeow
    return (catchCompose mainTry)

{-
{- Watch/Catch -}
----------------------------------------------------------------
type CatchCallback = Block -> Statement

parseTry :: Nesting -> Parser Block
parseTry nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowTry
    let nest = max nesting Nested
    return $ Lexer.IndentMany Nothing (return . Stack.fromList) (statements nest)

parseCatch :: Nesting -> Parser CatchCallback
parseCatch nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowCatch
    expr <- Mega.optional $ (lexeme . parens) parseExpr
    let makeBody = return . flip StmtTryCatch . (expr,) . Stack.fromList
    let nest = max nesting Nested
    return $ Lexer.IndentMany Nothing makeBody (statements nest)

parseTryCatch :: Nesting -> Parser Statement
parseTryCatch nesting = lexeme $ do
    tryBlock <- parseTry nesting
    catches  <- Mega.some (parseCatch nesting)
    let compose :: CatchCallback -> CatchCallback -> CatchCallback
        compose x acc = acc . Stack.singleton . x
    let foldedCatch = foldr1 compose catches
    parseEnd
    return $ foldedCatch tryBlock


----------------------------------------------------------------
{- Syntatic Sugar -}
----------------------------------------------------------------

{- When -}
-- Syntax is: when <expr> do <expr>
-- Parsed into a simple 'if' statement.
parseWhen :: Nesting -> Parser Statement
parseWhen _ = lexeme $ do
    let (start, end) = meowWhen
    (void . tryKeyword) start
    condition <- parensExpression
    (void . keyword) end
    body <- Stack.singleton . StmtExpr <$> parseExpr
    return (StmtIfElse condition body Stack.empty)


----------------------------------------------------------------
{- Utils -}
----------------------------------------------------------------
loopKeywordOutsideLoop :: Text -> String
loopKeywordOutsideLoop k = concat ["Cannot have '", Text.unpack k, "' statement outside of a loop!"]
-}


{-
module Mewlix.Parser.Statement
( statements
, root
) where

import Mewlix.Parser.AST
import Mewlix.Parser.Expr
import Mewlix.Parser.Utils
import Mewlix.Parser.Keywords
import Mewlix.Parser.Prim
import qualified Mewlix.Data.Stack as Stack
import Text.Megaparsec ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)

root :: Parser Block
root = do
    let parser = (Mega.many . lexemeMultiline) (statements Root)
    Stack.fromList <$> Mega.between whitespaceLn Mega.eof parser

{- Nesting -}
----------------------------------------------------------------
data Nesting =
      Root
    | Nested
    | NestedInLoop
    deriving (Eq, Ord, Show, Enum, Bounded)

-- to resolve nesting, do
-- a `max` b
-- the maximum nesting always propagates! for example
-- root `max` nestedinloop = nestedinloop


{- Parse statements: -}
----------------------------------------------------------------
statements :: Nesting -> Parser Statement
statements n = Mega.choice
    [ parseWhile n               <??> meowWhile
    , parseFor n                 <??> (\(x,_,_) -> x) meowFor
    , parseIfElse n              <??> meowIf
    , parseFunc n                <??> meowCatface
    , parseDeclaration n         <??> meowLocal
    , parseReturn n              <??> meowReturn
    , parseContinue n            <??> meowContinue
    , parseBreak n               <??> meowBreak
    , parseWhen n                <??> fst meowWhen
    , parseImport n              <??> fst meowTakes
    , parseTryCatch n            <??> Text.concat [ meowTry, "/", meowCatch ]
    , parseExpression n          <??> ("expression" :: String) ]

parseEnd :: Parser ()
parseEnd = whitespace >> (void . keyword) meowEnd
    <|> fail (concat ["Expected '", Text.unpack meowEnd, "': Block is unclosed instead!"])

parensExpression :: Parser Expr
parensExpression = (lexeme . parens) (whitespace >> parseExpr)

{- Expression -}
----------------------------------------------------------------
parseExpression :: Nesting -> Parser Statement
parseExpression _ = whitespace >> StmtExpr <$> parseExpr


{- Declaration -}
----------------------------------------------------------------
parseDeclaration :: Nesting -> Parser Statement
parseDeclaration _ = uncurry StmtDeclaration <$> declaration


{- While -}
----------------------------------------------------------------
parseWhile:: Nesting -> Parser Statement
parseWhile nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowWhile
    whitespace
    condition <- parensExpression
    let nest = max nesting NestedInLoop
    let makeBody = (<$ parseEnd) . StmtWhile condition . Stack.fromList
    return $ Lexer.IndentMany Nothing makeBody (statements nest)


{- If Else -}
----------------------------------------------------------------
type IfCallback = Block -> Statement

parseIf :: Nesting -> Parser IfCallback
parseIf nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowIf
    condition <- parensExpression
    let nest = max nesting Nested
    let makeBody = return . StmtIfElse condition. Stack.fromList
    return $ Lexer.IndentMany Nothing makeBody (statements nest)

parseElse :: Nesting -> Parser Block
parseElse nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowElse
    let nest = max nesting Nested
    return $ Lexer.IndentMany Nothing (return . Stack.fromList) (statements nest)

parseIfElse :: Nesting -> Parser Statement
parseIfElse nesting = do
    ifs         <- Mega.some (parseIf nesting)
    elseBlock   <- Mega.try (parseElse nesting) <|> return Stack.empty
    let compose :: IfCallback -> IfCallback -> IfCallback
        compose x acc = x . Stack.singleton . acc
    let foldedIf = foldr1 compose ifs
    parseEnd
    return $ foldedIf elseBlock


{- Functions -}
----------------------------------------------------------------
funParams :: Parser Params
funParams = (lexeme . parens) $ whitespace >> Stack.fromList <$> sepByComma (bilexeme keyText)

parseFunc :: Nesting -> Parser Statement
parseFunc _ = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . specialSymbol) meowCatface
    name <- lexeme funName
    args <- funParams
    let makeBody = (<$ parseEnd) . StmtFuncDef name args . Stack.fromList
    return $ Lexer.IndentMany Nothing makeBody (statements Nested)

funName :: Parser Expr
funName = whitespace >> lexeme exprFnKey

{- Return -}
----------------------------------------------------------------
parseReturn :: Nesting -> Parser Statement
parseReturn _ = lexeme $ do
    whitespace
    (void . keyword) meowReturn
    StmtReturn <$> parseExpr


{- Continue -}
----------------------------------------------------------------
parseContinue :: Nesting -> Parser Statement
parseContinue nesting = lexeme $ do
    whitespace
    (void . keyword) meowContinue
    case nesting of
        NestedInLoop -> return StmtContinue
        _            -> fail $ loopKeywordOutsideLoop meowContinue


{- Break -}
----------------------------------------------------------------
parseBreak :: Nesting -> Parser Statement
parseBreak nesting = lexeme $ do
    whitespace
    (void . keyword) meowBreak
    case nesting of
        NestedInLoop -> return StmtBreak
        _            -> fail $ loopKeywordOutsideLoop meowBreak


{- For Loop -}
----------------------------------------------------------------
parseFor :: Nesting -> Parser Statement
parseFor nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    let (start, middle, end) = meowFor
    let lifted = (lexeme . parens) liftedExpr
    let tryFirst = tryKeyword start >> lifted
    let getExp name = keyword name >> parensExpression
    (a, b, c) <- (,,) <$> tryFirst <*> getExp middle <*> getExp end
    let expressions = (a, b, c)
    let nest = max nesting NestedInLoop
    let makeBody = (<$ parseEnd) . StmtFor expressions . Stack.fromList
    return $ Lexer.IndentMany Nothing makeBody (statements nest)


{- Import -}
----------------------------------------------------------------
parseImport :: Nesting -> Parser Statement
parseImport nesting = lexeme $ do
    let (start, end) = meowTakes
    (void . tryKeyword) start
    (PrimStr filepath) <- lexeme parseStr
    let key = (void . tryKeyword) end >> lexeme keyText
    maybeKey <- Mega.optional key
    case nesting of
        Root -> return (StmtImport filepath maybeKey)
        _    -> fail "Import statements cannot be nested!"

{- Watch/Catch -}
----------------------------------------------------------------
type CatchCallback = Block -> Statement

parseTry :: Nesting -> Parser Block
parseTry nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowTry
    let nest = max nesting Nested
    return $ Lexer.IndentMany Nothing (return . Stack.fromList) (statements nest)

parseCatch :: Nesting -> Parser CatchCallback
parseCatch nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowCatch
    expr <- Mega.optional $ (lexeme . parens) parseExpr
    let makeBody = return . flip StmtTryCatch . (expr,) . Stack.fromList
    let nest = max nesting Nested
    return $ Lexer.IndentMany Nothing makeBody (statements nest)

parseTryCatch :: Nesting -> Parser Statement
parseTryCatch nesting = lexeme $ do
    tryBlock <- parseTry nesting
    catches  <- Mega.some (parseCatch nesting)
    let compose :: CatchCallback -> CatchCallback -> CatchCallback
        compose x acc = acc . Stack.singleton . x
    let foldedCatch = foldr1 compose catches
    parseEnd
    return $ foldedCatch tryBlock


----------------------------------------------------------------
{- Syntatic Sugar -}
----------------------------------------------------------------

{- When -}
-- Syntax is: when <expr> do <expr>
-- Parsed into a simple 'if' statement.
parseWhen :: Nesting -> Parser Statement
parseWhen _ = lexeme $ do
    let (start, end) = meowWhen
    (void . tryKeyword) start
    condition <- parensExpression
    (void . keyword) end
    body <- Stack.singleton . StmtExpr <$> parseExpr
    return (StmtIfElse condition body Stack.empty)


----------------------------------------------------------------
{- Utils -}
----------------------------------------------------------------
loopKeywordOutsideLoop :: Text -> String
loopKeywordOutsideLoop k = concat ["Cannot have '", Text.unpack k, "' statement outside of a loop!"]
-}
