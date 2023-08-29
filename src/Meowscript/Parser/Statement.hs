{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Parser.Statement
(
) where

import Meowscript.Parser.AST
import Meowscript.Parser.Expr
import Meowscript.Parser.Utils
import Meowscript.Parser.Keywords
import Meowscript.Parser.Prim
import Meowscript.Data.Stack (Stack)
import qualified Meowscript.Data.Stack as Stack
import Text.Megaparsec ((<|>), (<?>))
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.List as List
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Control.Monad (void)

root :: Parser [Statement]
root = Mega.between whitespaceLn Mega.eof (Mega.many (lexemeLn statements))

{- Parse statements: -}
----------------------------------------------------------------
statements :: Parser Statement
statements = Mega.choice
    [  parseWhile                <?> Text.unpack meowWhile
     , parseFor                  <?> (Text.unpack . \(x,_,_) -> x) meowFor
     , parseIfElse               <?> Text.unpack meowIf
     , parseFunc                 <?> Text.unpack meowCatface
     , parseReturn               <?> Text.unpack meowReturn
     , parseContinue             <?> Text.unpack meowContinue
     , parseBreak                <?> Text.unpack meowBreak
     , parseImport               <?> (Text.unpack . fst) meowTakes
     , parseTryCatch             <?> (Text.unpack . Text.concat) [ meowTry, meowCatch ]
     , parseExpression           <?> "expression" ]

parseEnd :: Parser ()
parseEnd = whitespace >> (void . keyword) meowEnd
    <|> fail (concat ["Expected '", Text.unpack meowEnd,
                "': Block is unclosed instead!"])

parensExpression :: Parser Expr
parensExpression = (lexeme . parens) (whitespace >> parseExpr)

parseExpression :: Parser Statement
parseExpression = whitespace >> StmtExpr <$> parseExpr


{- While -}
----------------------------------------------------------------
parseWhile:: Parser Statement
parseWhile = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . keyword) meowWhile
    whitespace
    condition <- parensExpression
    let makeBody = (<$ parseEnd) . StmtWhile condition . Stack.fromList
    return (Lexer.IndentMany Nothing makeBody statements)


{- If Else -}
----------------------------------------------------------------
data MeowIf = MeowIf Expr Block deriving (Show)

parseIf :: Parser (Expr, Block)
parseIf = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . keyword) meowIf
    condition <- parensExpression
    let makeBody = return . (condition,) . Stack.fromList
    return (Lexer.IndentMany Nothing makeBody statements)

parseElse :: Parser Block
parseElse = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . keyword) meowElse
    return (Lexer.IndentMany Nothing (return . Stack.fromList) statements)

-- Allows nested if/else!
elseIf :: Parser Block
elseIf = Mega.try parseElse <|> fmap Stack.singleton parseIfElse <|> return Stack.empty

parseIfElse :: Parser Statement
parseIfElse = lexeme $ do
    (cond, ifBody) <- parseIf
    elseBody <- elseIf
    parseEnd
    return (StmtIf cond ifBody elseBody)

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
funName = makeExprParser (ExprPrim <$> lexeme parsePrim)
    [ [ Postfix parseDotOp                   ] ]


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
    let getExp name = (void . keyword) name >> parensExpression
    let tryFirst = Mega.try (getExp start)
    (a, b, c) <- (,,) <$> tryFirst <*> getExp middle <*> getExp end
    let expressions = (a, b, c)
    let makeBody = (<$ parseEnd) . StmtFor expressions . Stack.fromList
    return $ Lexer.IndentMany Nothing makeBody statements


{- Import -}
----------------------------------------------------------------
parseImport :: Parser Statement
parseImport = lexeme $ do
    let (start, end) = meowTakes
    (void . Mega.try . keyword) start
    (PrimStr filepath) <- lexeme parseStr
    let key = (void . Mega.try . keyword) end >> lexeme keyText
    maybeKey <- Mega.optional key
    return (StmtImport filepath maybeKey)

{- Watch/Catch -}
----------------------------------------------------------------
parseTry :: Parser Block
parseTry = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . Mega.try . keyword) meowTry
    return $ Lexer.IndentMany Nothing (return . Stack.fromList) statements

parseCatch :: Parser CatchBlock
parseCatch = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . Mega.try . keyword) meowCatch
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
