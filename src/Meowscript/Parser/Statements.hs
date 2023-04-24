{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Statements
( root
, statements
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
    , Mega.try parseIfElse
    , Mega.try parseFunc
    , exprS
    , fail "Invalid token!" ]

asBlock :: [Statement] -> Parser Statement
asBlock xs = return $ SAll xs

asIf :: Expr -> [Statement] -> Parser Statement
asIf e xs = do
    whitespace
    void $ MChar.string "leave"
    return $ SOnlyIf e xs

asWhile :: Expr -> [Statement] -> Parser Statement
asWhile e xs = do
    whitespace
    void $ MChar.string "leave"
    return $ SWhile e xs

condition :: Parser Expr
condition = (lexeme . bars) parseExpr'

exprS :: Parser Statement
exprS = SExpr <$> parseExpr'

parseIf :: Parser Statement
parseIf = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ MChar.string "mew?"
    whitespace
    c <- condition
    return (Lexer.IndentMany Nothing (asIf c) statements)

bAsIf :: Expr -> [Statement] -> Parser ([Statement] -> Statement)
bAsIf e xs = return $ SIfElse e xs

bAsElse :: ([Statement] -> Statement) -> [Statement] -> Parser Statement
bAsElse f xs = do
    void $ MChar.string "leave"
    return (f xs)

parseIfElse :: Parser Statement
parseIfElse = do
    x <- parseBIf
    parseBElse x

parseBIf :: Parser ([Statement] -> Statement)
parseBIf = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ MChar.string "mew?"
    whitespace
    c <- condition
    return (Lexer.IndentMany Nothing (bAsIf c) statements)

parseBElse :: ([Statement] -> Statement) -> Parser Statement
parseBElse f = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ MChar.string "hiss!"
    return (Lexer.IndentMany Nothing (bAsElse f) statements)


parseWhile:: Parser Statement
parseWhile = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ MChar.string "meowwww"
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



funName :: Parser Text.Text
funName = atomName <$> lexeme (whitespace >> parseAtom)

funArgs :: Parser [Text.Text]
funArgs = (lexeme . bars) $ do
    whitespace
    args <- Mega.sepBy (whitespace >> parseAtom) (MChar.char ',')
    whitespace
    return $ atomName <$> args

asFunc :: Text.Text -> [Text.Text] -> [Statement] -> Parser Statement
asFunc name args xs = do
    whitespace
    void $ MChar.string "leave"
    return $ SFuncDef name args xs

parseFunc :: Parser Statement
parseFunc = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ MChar.string "purr"
    name <- funName
    args <- funArgs
    return (Lexer.IndentMany Nothing (asFunc name args) statements)
