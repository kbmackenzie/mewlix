{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Parser.Statement
( statements
, root
) where

import Meowscript.Parser.AST
import Meowscript.Parser.Expr
import Meowscript.Parser.Utils
import Meowscript.Parser.Keywords
import Meowscript.Parser.Prim
import Meowscript.Data.Stack (Stack)
import qualified Meowscript.Data.Stack as Stack
import Text.Megaparsec ((<|>))
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)

root :: Parser [Statement]
root = Mega.between whitespaceLn Mega.eof (Mega.many (lexemeLn statements))

{- Parse statements: -}
----------------------------------------------------------------
statements :: Parser Statement
statements = Mega.choice
    [ parseWhile                <??> meowWhile
    , parseFor                  <??> (\(x,_,_) -> x) meowFor
    , parseIfElse               <??> meowIf
    , parseFunc                 <??> meowCatface
    , parseDeclaration          <??> meowLocal
    , parseReturn               <??> meowReturn
    , parseContinue             <??> meowContinue
    , parseBreak                <??> meowBreak
    , parseWhen                 <??> fst meowWhen
    , parseImport               <??> fst meowTakes
    , parseTryCatch             <??> Text.concat [ meowTry, "/", meowCatch ]
    , parseExpression           <??> ("expression" :: String) ]

parseEnd :: Parser ()
parseEnd = whitespace >> (void . keyword) meowEnd
    <|> fail (concat ["Expected '", Text.unpack meowEnd,
                "': Block is unclosed instead!"])

parensExpression :: Parser Expr
parensExpression = (lexeme . parens) (whitespace >> parseExpr)

parseExpression :: Parser Statement
parseExpression = whitespace >> StmtExpr <$> parseExpr


{- Declaration -}
----------------------------------------------------------------
parseDeclaration :: Parser Statement
parseDeclaration = uncurry StmtDeclaration <$> declaration


{- While -}
----------------------------------------------------------------
parseWhile:: Parser Statement
parseWhile = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowWhile
    whitespace
    condition <- parensExpression
    let makeBody = (<$ parseEnd) . StmtWhile condition . Stack.fromList
    return (Lexer.IndentMany Nothing makeBody statements)


{- If Else -}
----------------------------------------------------------------
data MeowIf = MeowIf Expr Block deriving (Show)

parseIf :: Parser (Expr, Block)
parseIf = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowIf
    condition <- parensExpression
    let makeBody = return . (condition,) . Stack.fromList
    return (Lexer.IndentMany Nothing makeBody statements)

parseElse :: Parser Block
parseElse = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowElse
    return (Lexer.IndentMany Nothing (return . Stack.fromList) statements)

-- Allows nested if/else!
elseIf :: Parser Block
elseIf = Mega.try parseElse <|> fmap Stack.singleton parseIfElse <|> return Stack.empty

parseIfElse :: Parser Statement
parseIfElse = lexeme $ do
    (cond, ifBody) <- parseIf
    elseBody <- elseIf
    parseEnd
    return (StmtIfElse cond ifBody elseBody)


{- Functions -}
----------------------------------------------------------------
funParams :: Parser Params
funParams = (lexeme . parens) $ whitespace >> Stack.fromList <$> sepByComma (bilexeme keyText)

parseFunc :: Parser Statement
parseFunc = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . specialSymbol) meowCatface
    name <- lexeme funName
    args <- funParams
    let makeBody = (<$ parseEnd) . StmtFuncDef name args . Stack.fromList
    return (Lexer.IndentMany Nothing makeBody statements)

funName :: Parser Expr
funName = whitespace >> exprL

{- Return -}
----------------------------------------------------------------
parseReturn :: Parser Statement
parseReturn = lexeme $ do
    whitespace
    (void . keyword) meowReturn
    StmtReturn <$> parseExpr


{- Continue -}
----------------------------------------------------------------
parseContinue :: Parser Statement
parseContinue = lexeme $ do
    whitespace
    StmtContinue <$ (void . keyword) meowContinue


{- Break -}
----------------------------------------------------------------
parseBreak :: Parser Statement
parseBreak = lexeme $ do
    whitespace
    StmtBreak <$ (void . keyword) meowBreak


{- For Loop -}
----------------------------------------------------------------
parseFor :: Parser Statement
parseFor = lexeme . Lexer.indentBlock whitespaceLn $ do
    let (start, middle, end) = meowFor
    let lifted = parens liftedExpr
    let tryFirst = tryKeyword start >> lifted
    let getExp name = keyword name >> parensExpression
    (a, b, c) <- (,,) <$> tryFirst <*> getExp middle <*> getExp end
    let expressions = (a, b, c)
    let makeBody = (<$ parseEnd) . StmtFor expressions . Stack.fromList
    return $ Lexer.IndentMany Nothing makeBody statements


{- Import -}
----------------------------------------------------------------
parseImport :: Parser Statement
parseImport = lexeme $ do
    let (start, end) = meowTakes
    (void . tryKeyword) start
    (PrimStr filepath) <- lexeme parseStr
    let key = (void . tryKeyword) end >> lexeme keyText
    maybeKey <- Mega.optional key
    return (StmtImport filepath maybeKey)

{- Watch/Catch -}
----------------------------------------------------------------
parseTry :: Parser Block
parseTry = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowTry
    return $ Lexer.IndentMany Nothing (return . Stack.fromList) statements

parseCatch :: Parser CatchBlock
parseCatch = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowCatch
    expr <- Mega.optional $ (lexeme . parens) parseExpr
    let makeBody = return . (expr,) . Stack.fromList
    return $ Lexer.IndentMany Nothing makeBody statements

manyCatch :: Block -> Parser Statement
manyCatch tryblock = do
    catchblock <- parseCatch
    let expr = StmtTryCatch tryblock catchblock
    manyCatch (Stack.singleton expr) <|> (expr <$ parseEnd)

parseTryCatch :: Parser Statement
parseTryCatch = lexeme $ do
    tryBlock <- parseTry
    Mega.try (manyCatch tryBlock) <|> return (StmtTryCatch tryBlock (Nothing, Stack.empty))


----------------------------------------------------------------
{- Syntatic Sugar -}
----------------------------------------------------------------

{- When -}
-- Syntax is: when <expr> do <expr>
-- Parsed into a simple 'if' statement.
parseWhen :: Parser Statement
parseWhen = lexeme $ do
    let (start, end) = meowWhen
    (void . tryKeyword) start
    condition <- parensExpression
    (void . keyword) end
    body <- Stack.singleton . StmtExpr <$> parseExpr
    return (StmtIfElse condition body Stack.empty)
