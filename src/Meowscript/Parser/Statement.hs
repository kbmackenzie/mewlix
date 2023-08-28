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
    return (Lexer.IndentMany Nothing ((<$ parseEnd) . StmtWhile condition) statements)


{- If Else -}
----------------------------------------------------------------
data MeowIf = MeowIf Expr Block deriving (Show)

parseIf :: Parser (Expr, Block)
parseIf = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . keyword) meowIf
    condition <- parensExpression
    return (Lexer.IndentMany Nothing (return . (condition,)) statements)

parseElse :: Parser [Statement]
parseElse = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . keyword) meowElse
    return (Lexer.IndentMany Nothing return statements)

-- Allows nested if/else!
elseIf :: Parser [Statement]
elseIf = Mega.try parseElse <|> fmap List.singleton parseIfElse <|> return []

parseIfElse :: Parser Statement
parseIfElse = lexeme $ do
    (cond, ifBody) <- parseIf
    elseBody <- elseIf
    parseEnd
    return (StmtIf cond ifBody elseBody)

{- Functions -}
----------------------------------------------------------------
funParams :: Parser [Text.Text]
funParams = (lexeme . parens) $ whitespace >> sepByComma (bilexeme keyText)

parseFunc :: Parser Statement
parseFunc = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . specialSymbol) meowCatface
    name <- lexeme funName
    args <- funParams
    return (Lexer.IndentMany Nothing ((<$ parseEnd) . StmtFuncDef name args) statements)

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
    return $ Lexer.IndentMany Nothing ((<$ parseEnd) . StmtFor expressions) statements


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
parseTry :: Parser [Statement]
parseTry = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . Mega.try . keyword) meowTry
    return $ Lexer.IndentMany Nothing return statements

parseCatch :: Parser CatchBlock
parseCatch = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . Mega.try . keyword) meowCatch
    expr <- Mega.optional $ (lexeme . parens) parseExpr
    return $ Lexer.IndentMany Nothing (return . (expr,)) statements

manyCatch :: Block -> Parser Statement
manyCatch tryblock = do
    catchblock <- parseCatch
    let expr = StmtTryCatch tryblock catchblock
    manyCatch [expr] <|> (expr <$ parseEnd)

parseTryCatch :: Parser Statement
parseTryCatch = lexeme $ do
    tryBlock <- parseTry
    Mega.try (manyCatch tryBlock) <|> return (StmtTryCatch tryBlock (Nothing, []))
