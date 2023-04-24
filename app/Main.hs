{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Meowscript.Parser.Expr
import Meowscript.Core.AST
import Meowscript.Parser.Core
import Meowscript.Parser.Statements
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
import Control.StopWatch (stopWatch)
import Data.Either (fromRight)

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

--unitTesst :: String -> Text.Text -> Either (Mega.ParseErrorBundle Text.Text Void) [Statement]
--unitTesst = Mega.parse (Mega.between whitespace Mega.eof manyStatements)

--aaa :: String -> Text.Text -> Either (Mega.ParseErrorBundle Text.Text Void) [Statement]
--aaa = Mega.parse manyStatements

aaab :: String -> Text.Text -> Either (Mega.ParseErrorBundle Text.Text Void) Statement
aaab = Mega.parse root 

parseS :: IO Statement
parseS = do
    let path = "C:\\Users\\ianvi\\Desktop\\example1_expr.txt"
    txt <- TextIO.readFile path
    let exp' = aaab "" txt
    {-let x = case exp' of
            (Right e) -> show e
            (Left e) -> show e-}
    return $ fromRight (SAll []) exp'

main :: IO ()
main = do
    (tok, time) <- stopWatch parseS
    print tok
    print time
