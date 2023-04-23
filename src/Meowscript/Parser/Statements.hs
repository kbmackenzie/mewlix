{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Statements
(
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import qualified Data.Text as Text
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- Placeholders. Replace 'dummy' with proper functions whenever you see it.
dummy :: Parser Expr
dummy = return $ EPrim MeowLonely
dummyStmt :: Parser Statement
dummyStmt = return $ SPrim MeowLonely

condition :: Parser Expr
condition = (lexeme . bars) (whitespace >> dummy)

parseIf :: Parser Statement
parseIf = (lexeme . meowDiv "if") $ do
    whitespace
    cond <- condition
    whitespace
    ifBody <- Mega.manyTill dummyStmt (keyword "else")
    elseBody <- Mega.many dummyStmt
    return $ SIfElse cond ifBody elseBody

parseWhile :: Parser Statement
parseWhile = (lexeme . meowDiv "roll") $ do
    whitespace
    cond <- condition
    whitespace
    body <- Mega.many dummyStmt
    return $ SWhile cond body
