{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Statements
( --condition
root
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

root :: Parser Statement
root = SAll <$> Mega.many statements

statements :: Parser Statement
statements = Mega.choice
    [ Mega.try parseIf
    , Mega.try parseWhile
    , exprS ]

asBlock :: [Statement] -> Parser Statement
asBlock xs = return $ SAll xs

asIf :: Expr -> [Statement] -> Parser Statement
asIf e xs = return $ SOnlyIf e xs

asWhile :: Expr -> [Statement] -> Parser Statement
asWhile e xs = return $ SWhile e xs

condition :: Parser Expr
condition = (lexeme . bars) parseExpr'

exprS :: Parser Statement
exprS = SExpr <$> parseExpr'

parseIf :: Parser Statement
parseIf = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ MChar.string "meowmeow"
    whitespace
    c <- condition
    return (Lexer.IndentMany Nothing (asIf c) statements)

parseWhile:: Parser Statement
parseWhile = lexeme $ Lexer.indentBlock whitespaceLn $ do
    whitespace
    c <- condition
    return (Lexer.IndentMany Nothing (asWhile c) statements)


dummyN :: Parser Int
dummyN = let p = return (Lexer.IndentMany Nothing dummyComb dummy)
         in Lexer.indentBlock whitespaceLn p

dummy :: Parser Int
dummy = return 0

dummyComb :: [Int] -> Parser Int
dummyComb xs = do
    let x = sum xs
    return x
