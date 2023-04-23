{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Statements
(
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Meowscript.Parser.Expr
import qualified Data.Text as Text
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer

condition :: Parser Expr
condition = (lexeme . bars) (whitespace >> parseExpr)

parseIf :: Parser Statement
parseIf = (lexeme . meowDiv "mew?") $ do
    whitespace
    cond <- condition
    whitespace
    ifBody <- Mega.between whitespace (MChar.string "else") exprStmt
    SIfElse cond ifBody <$> exprStmt

parseWhile :: Parser Statement
parseWhile = (lexeme . meowDiv "meowmeow") $ do
    whitespace
    cond <- condition
    whitespace
    SWhile cond <$> exprStmt
