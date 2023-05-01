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
statements = Mega.choice
    [ parseWhile                <?> "while"
    , parseIfElse               <?> "if/else"
    , Mega.try parseFunc        <?> "function"
    , Mega.try parseReturn      <?> "return"
    , Mega.try parseContinue    <?> "continue"
    , Mega.try parseBreak       <?> "break"
    , parseExpression           <?> "expression"
    , fail "Invalid token!" ]

parseEnd :: Parser ()
parseEnd = whitespace >> (void . keyword) meowEnd

parseCondition :: Parser Expr
parseCondition = (lexeme . bars) parseExpr'

parseExpression :: Parser Statement
parseExpression = whitespace >> SExpr <$> parseExpr'


{- While -}

parseWhile:: Parser Statement
parseWhile = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . keyword) meowWhile
    whitespace
    condition <- parseCondition
    Lexer.IndentMany Nothing (return . SWhile condition) statements <$ parseEnd

{- If Else -}

parseIf :: Parser (Expr, [Statement])
parseIf = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . lexeme . keyword) meowIf
    condition <- parseCondition
    return (Lexer.IndentMany Nothing (return . (condition,)) statements)

parseElse :: Parser [Statement]
parseElse = lexeme . Lexer.indentBlock whitespaceLn $ do
    (Mega.try . void . lexeme . keyword) meowElse
    return (Lexer.IndentMany Nothing return statements)

parseIfElse :: Parser Statement
parseIfElse = do
    (cond, ifBody) <- parseIf
    let elseS = parseElse <&> SIfElse cond ifBody
    let onlyIf = return (SOnlyIf cond ifBody)
    (elseS <|> onlyIf) <* parseEnd

{- Functions -}

funArgs :: Parser [Text.Text]
funArgs = (lexeme . bars . sepByComma) (whitespace >> keyText)

parseFunc :: Parser Statement
parseFunc = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . keyword) meowCatface
    name <- lexeme keyText
    args <- funArgs
    Lexer.IndentMany Nothing (return . SFuncDef name args) statements <$ parseEnd


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
