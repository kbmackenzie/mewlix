{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Meowscript.Parser.Expr
import Meowscript
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
import Control.Monad.State
import Control.Monad.Reader 
import Control.Monad.Identity (Identity)

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

parseS :: Text.Text -> IO Statement
parseS txt = do
    let !exp' = aaab "" txt
    return $ fromRight (SAll []) exp'

parseE :: Text.Text -> IO ()
parseE txt = do
    let !exp' = aaab "" txt
    print exp'

main' :: IO ()
main' = do 
    let path = "C:\\Users\\ianvi\\Desktop\\example1_expr.txt"
    !txt <- TextIO.readFile path
    (tok, time) <- stopWatch (parseE txt)
    print tok
    print time

main :: IO ()
main = do 
    let path = "C:\\Users\\ianvi\\Desktop\\example1_expr.txt"
    main'
    x <- runBasic path
    print x


{- Reader - Experimentation -}
{-
readerExp :: [Int] -> Reader [Int] Int
readerExp [] = do
    sum env
readerExp (x:xs) = do
    env <- ask
    let env' = x:env
    local (const env') (readerExp xs)

readerAddOne :: Reader [Int] Int
readerAddOne = do
    env <- ask
    let env' = 1:env
    local (const env') (asks sum)

readerCore :: Reader [Int] [Int]
readerCore = do
    x <- readerExp [3, 3, 3]
    return [x]

readerRun :: [Int]
readerRun = runReader readerCore [] -}

{- State -- Experimentation -}
stateExp :: [Int] -> State [Int] Int
stateExp [] = do
    void stateAddOne
    gets sum
stateExp (x:xs) = do
    s <- get
    put (x:s)
    stateExp xs

stateAddOne :: State [Int] Int
stateAddOne = do
    s <- get
    put (1:s)
    return 0

stateRun :: Int
stateRun = evalState (stateExp [3, 3, 3]) []


main''' :: IO ()
main''' = do
    let x = stateRun
    print x
