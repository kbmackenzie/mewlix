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
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)
import Data.Functor((<&>))

root :: Parser Statement
root = SAll <$> Mega.between whitespaceLn Mega.eof (Mega.many (lexemeLn statements))

statements :: Parser Statement
statements = Mega.choice $ Mega.try <$>
    [  parseWhile                <?> "while"
     , parseFor                  <?> "for"
     , parseIfElse               <?> "if/else"
     , parseFunc                 <?> "function"
     , parseReturn               <?> "return"
     , parseContinue             <?> "continue"
     , parseBreak                <?> "break"
     , parseTake                 <?> "take"
     , parseExpression           <?> "expression" ]

parseEnd :: Parser ()
parseEnd = whitespace >> (void . keyword) meowEnd

barsExpression :: Parser Expr
barsExpression = (lexeme . bars) parseExpr'

parseExpression :: Parser Statement
parseExpression = whitespace >> SExpr <$> parseExpr'


{- While -}

parseWhile:: Parser Statement
parseWhile = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . keyword) meowWhile
    whitespace
    condition <- barsExpression
    return (Lexer.IndentMany Nothing ((<$ parseEnd) . SWhile condition) statements)

{- If Else -}

parseIf :: Parser (Expr, [Statement])
parseIf = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . lexeme . keyword) meowIf
    condition <- barsExpression
    return (Lexer.IndentMany Nothing (return . (condition,)) statements)

parseElse :: Parser [Statement]
parseElse = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . lexeme . keyword) meowElse
    return (Lexer.IndentMany Nothing return statements)

parseIfElse :: Parser Statement
parseIfElse = lexeme $ do
    (cond, ifBody) <- parseIf
    let elseS = parseElse <&> SIfElse cond ifBody
    let onlyIf = return (SIf cond ifBody)
    (Mega.try elseS <|> onlyIf) <* parseEnd

{- Functions -}

funArgs :: Parser [Text.Text]
funArgs = (lexeme . bars . sepByComma) (flexeme keyText)

parseFunc :: Parser Statement
parseFunc = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . keyword) meowCatface
    name <- lexeme keyText
    args <- funArgs
    return (Lexer.IndentMany Nothing ((<$ parseEnd) . SFuncDef name args) statements)


{- Return -}

parseReturn :: Parser Statement
parseReturn = lexeme $ do
    whitespace
    (void . keyword) meowReturn
    SReturn <$> parseExpr'

{- Continue -}
parseContinue :: Parser Statement
parseContinue = lexeme $ do
    whitespace
    SContinue <$ (void . keyword) meowContinue

{- Break -}

parseBreak :: Parser Statement
parseBreak = lexeme $ do
    whitespace
    SBreak <$ (void . keyword) meowBreak


{- For Loop -}

parseFor :: Parser Statement
parseFor = lexeme . Lexer.indentBlock whitespaceLn $ do
    let (start, middle, end) = meowFor
    let getExp name = (void . lexeme . keyword) name >> barsExpression
    let first = Mega.try (getExp start)
    (a, b, c) <- (,,) <$> first <*> getExp middle <*> getExp end
    let expressions = (a, b, c)
    return $ Lexer.IndentMany Nothing ((<$ parseEnd) . SFor expressions) statements

{- Import -}

parseTake :: Parser Statement
parseTake = lexeme $ do
    let (start, end) = meowTakes
    (void . Mega.try . lexeme . keyword) start
    (MeowString moduleName) <- lexeme parseStr
    let key = (void . Mega.try . lexeme . keyword) end >> lexeme keyText
    maybeKey <- Mega.optional key
    return (SImport moduleName maybeKey)
