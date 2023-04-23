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

condition :: Parser Expr
condition = Mega.between (MChar.char '|') (MChar.char '|') (whitespace >> lexeme parseExpr)

manyStatements :: Parser [Statement]
manyStatements = Mega.some parseStatement

parseStatement :: Parser Statement
parseStatement = Mega.choice
    [ Mega.try parseOnlyIf
    , Mega.try parseWhile 
    --, Mega.try parseReturn
    , placeholder ]

placeholder :: Parser Statement
placeholder = do
    x <- lexeme parseExpr
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
    return $ SOnlyIf cond []

parseWhile :: Parser Statement
parseWhile = (lexeme . meowDiv "meowmeow") $ do
    whitespace
    cond <- condition
    whitespace
    return $ SWhile cond []

parseReturn :: Parser Statement
parseReturn = lexeme $ do
    whitespace
    void $ MChar.string "return"
    whitespace
    SReturn <$> parseExpr
