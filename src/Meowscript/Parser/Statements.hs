{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TupleSections #-}

module Meowscript.Parser.Statements
( root
, statements
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Meowscript.Parser.Expr
import Meowscript.Parser.Keywords
import Text.Megaparsec ((<|>), (<?>))
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Control.Monad (void)
import Data.Functor((<&>))

root :: Parser [Statement]
root = Mega.between whitespaceLn Mega.eof (Mega.many (lexemeLn statements))

statements :: Parser Statement
statements = Mega.choice
    [  parseWhile                <?> "purr"
     , parseFor                  <?> "take"
     , parseIfElse               <?> "mew?"
     , parseFunc                 <?> "=^.x.^="
     , parseReturn               <?> "bring"
     , parseContinue             <?> "catnap"
     , parseBreak                <?> "run off"
     , parseImport               <?> "takes"
     , parseExpression           <?> "expression" ]

parseEnd :: Parser ()
parseEnd = whitespace >> (void . keyword) meowEnd

parensExpression :: Parser Expr
parensExpression = (lexeme . parens) (whitespace >> parseExpr')

parseExpression :: Parser Statement
parseExpression = whitespace >> StmExpr <$> parseExpr'


{- While -}

parseWhile:: Parser Statement
parseWhile = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . keyword) meowWhile
    whitespace
    condition <- parensExpression
    return (Lexer.IndentMany Nothing ((<$ parseEnd) . StmWhile condition) statements)

{- If Else -}

parseIf :: Parser (Expr, [Statement])
parseIf = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . lexeme . keyword) meowIf
    condition <- parensExpression
    return (Lexer.IndentMany Nothing (return . (condition,)) statements)

parseIf' :: Parser MeowIf
parseIf' = parseIf >>= \(cond, block) -> return (MeowIf cond block)

parseElse :: Parser [Statement]
parseElse = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . lexeme . keyword) meowElse
    return (Lexer.IndentMany Nothing return statements)

parseIfElse :: Parser Statement
parseIfElse = lexeme $ do
    ifs <- Mega.many parseIf'
    let elseS = parseElse <&> StmIfElse ifs
    let onlyIf = return (StmIf ifs)
    (Mega.try elseS <|> onlyIf) <* parseEnd

{- Functions -}

funParams :: Parser [Text.Text]
funParams = (lexeme . parens) $ whitespace >> sepByComma (flexeme keyText)

parseFunc :: Parser Statement
parseFunc = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . keyword) meowCatface
    name <- lexeme funName
    args <- funParams
    return (Lexer.IndentMany Nothing ((<$ parseEnd) . StmFuncDef name args) statements)

funName :: Parser Expr
funName = makeExprParser (ExpPrim <$> lexeme parsePrim) [[ InfixL (ExpTrail <$ symbol ".") ]]

{- Return -}

parseReturn :: Parser Statement
parseReturn = lexeme $ do
    whitespace
    (void . keyword) meowReturn
    StmReturn <$> parseExpr'

{- Continue -}
parseContinue :: Parser Statement
parseContinue = lexeme $ do
    whitespace
    StmContinue <$ (void . keyword) meowContinue

{- Break -}

parseBreak :: Parser Statement
parseBreak = lexeme $ do
    whitespace
    StmBreak <$ (void . keyword) meowBreak


{- For Loop -}

parseFor :: Parser Statement
parseFor = lexeme . Lexer.indentBlock whitespaceLn $ do
    let (start, middle, end) = meowFor
    let getExp name = (void . lexeme . keyword) name >> parensExpression
    let first = Mega.try (getExp start)
    (a, b, c) <- (,,) <$> first <*> getExp middle <*> getExp end
    let expressions = (a, b, c)
    return $ Lexer.IndentMany Nothing ((<$ parseEnd) . StmFor expressions) statements

{- Import -}

parseImport :: Parser Statement
parseImport = lexeme $ do
    let (start, end) = meowTakes
    (void . Mega.try . lexeme . keyword) start
    (MeowString x) <- lexeme parseStr 
    let filepath = Text.unpack x
    let key = (void . Mega.try . lexeme . keyword) end >> lexeme keyText
    maybeKey <- Mega.optional key
    return (StmImport filepath maybeKey)
