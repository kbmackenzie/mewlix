{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Meowscript.Parser.Expr
import Meowscript.Core.AST
import Meowscript.Parser.Core
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Text.Megaparsec as Mega
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Void (Void)
import Data.Either (fromRight, fromLeft)
import Control.Monad (void)
import Data.Char (isAlphaNum)

exprTest :: String -> Text.Text -> Either (Mega.ParseErrorBundle Text.Text Void) Expr
exprTest = Mega.parse (Mega.between whitespace Mega.eof parseExpr)

{-
parseManyExpr :: Parser [[Expr]]
parseManyExpr = Mega.many (Mega.manyTill parseExpr (MChar.char ';'))

exprMany :: String -> Text.Text -> Either (Mega.ParseErrorBundle Text.Text Void) [[Expr]]
exprMany = Mega.parse (Mega.between whitespace Mega.eof parseManyExpr)
-}

parseMany :: Parser [Expr]
parseMany = Mega.sepEndBy (whitespace >> parseExpr) MChar.newline --(symbol ";")

exprMany :: String -> Text.Text -> Either (Mega.ParseErrorBundle Text.Text Void) [Expr]
exprMany = Mega.parse (Mega.between whitespace Mega.eof parseMany)

main :: IO ()
main = do
    let path = "C:\\Users\\ianvi\\Desktop\\example1_expr.txt"
    txt <- TextIO.readFile path
    let exp' = exprMany "" txt
    let x = case exp' of
            (Right e) -> show e
            (Left e) -> show e
    putStrLn x
