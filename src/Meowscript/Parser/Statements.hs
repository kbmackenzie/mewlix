{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Statements
( condition
, manyStatements
, parseStatement
, parseOnlyIf
, parseIfElse
, parseIf
, parseWhile
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Meowscript.Parser.Expr
import qualified Data.Text as Text
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)
import Text.Megaparsec.Char.Lexer (nonIndented)

root :: Parser [Statement]
root = nonIndented children

children :: Parser Statement


condition :: Parser Expr
condition = Mega.between (MChar.char '|') (MChar.char '|') parseExpr'

manyStatements :: Parser [Statement]
manyStatements = Mega.some parseStatement

parseStatement :: Parser Statement
parseStatement = Mega.choice
    [ Mega.try parseOnlyIf
    , Mega.try parseWhile 
    , Mega.try parseReturn
    , expressionStatement ]

parseExpr' :: Parser Expr
parseExpr' = lexeme (whitespace >> parseExpr)

expressionStatement :: Parser Statement
expressionStatement = do
    x <- parseExpr'
    return $ SExpr [x]

parseIf :: Parser Statement
parseIf = Mega.choice
    [ parseOnlyIf 
    , parseIfElse ]
    
parseIfElse :: Parser Statement
parseIfElse = (lexeme . meowDiv "mew?") $ do
    whitespace
    cond <- condition
    whitespace
    ifBody <- Mega.between whitespace (MChar.string "else") manyStatements
    SIfElse cond ifBody <$> manyStatements

parseOnlyIf :: Parser Statement
parseOnlyIf = (lexeme . meowDiv "mew?") $ do
    whitespace
    cond <- condition
    whitespace
    SOnlyIf cond <$> manyStatements

{-
parseWhile :: Parser Statement
parseWhile = (lexeme . meowDiv "meowmeow") $ do
    whitespace
    cond <- condition
    x <- manyStatements
    return $ SWhile cond x ""
-}

parseWhile :: Parser Statement
parseWhile = lexeme $ do
    whitespace
    void $ MChar.string "meowmeow"
    whitespace
    cond <- condition
    x <- Mega.between whitespace (MChar.string "leave") parseExpr'
    return $ SWhile cond [SExpr [x]] ""

parseReturn :: Parser Statement
parseReturn = lexeme $ do
    whitespace
    void $ lexeme $ MChar.string "return"
    SReturn <$> parseExpr
