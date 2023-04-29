{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Statements
( root
, statements
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Meowscript.Parser.Expr
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)

root :: Parser Statement
root = SAll <$> Mega.between whitespaceLn Mega.eof (Mega.many (lexemeLn statements))

statements :: Parser Statement
statements = Mega.choice
    [ Mega.try parseIf
    , Mega.try parseWhile
    , Mega.try parseIfElse
    , Mega.try parseFunc
    , Mega.try parseReturn
    , Mega.try parseContinue
    , Mega.try parseBreak
    , exprS
    , fail "Invalid token!" ]

asLeave :: ([Statement] -> Statement) -> [Statement] -> Parser Statement
asLeave f xs = do
    whitespace
    void $ keyword "leave"
    return (f xs)

condition :: Parser Expr
condition = (lexeme . bars) parseExpr'

exprS :: Parser Statement
exprS = whitespace >> SExpr <$> parseExpr'


{- If -}

asIf :: Expr -> [Statement] -> Parser Statement
asIf e = asLeave (SOnlyIf e)

parseIf :: Parser Statement
parseIf = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ keyword "mew?"
    whitespace
    c <- condition
    return (Lexer.IndentMany Nothing (asIf c) statements)


{- While -}

asWhile :: Expr -> [Statement] -> Parser Statement
asWhile e = asLeave (SWhile e)

parseWhile:: Parser Statement
parseWhile = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ keyword "scratch"
    whitespace
    c <- condition
    return (Lexer.IndentMany Nothing (asWhile c) statements)



{- If Else -}

bAsIf :: Expr -> [Statement] -> Parser ([Statement] -> Statement)
bAsIf e xs = return $ SIfElse e xs

bAsElse :: ([Statement] -> Statement) -> [Statement] -> Parser Statement
bAsElse f xs = do
    void $ keyword "leave"
    return (f xs)

parseIfElse :: Parser Statement
parseIfElse = do
    x <- parseBIf
    parseBElse x

parseBIf :: Parser ([Statement] -> Statement)
parseBIf = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ keyword "mew?"
    whitespace
    c <- condition
    return (Lexer.IndentMany Nothing (bAsIf c) statements)

parseBElse :: ([Statement] -> Statement) -> Parser Statement
parseBElse f = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ keyword "hiss!"
    return (Lexer.IndentMany Nothing (bAsElse f) statements)


{- Functions -}

funName :: Parser Text.Text
funName = lexeme (whitespace >> keyText)

funArgs :: Parser [Text.Text]
funArgs = (lexeme . bars) $ do
    whitespace
    args <- Mega.sepBy (whitespace >> keyText) (MChar.char ',')
    whitespace
    return args

asFunc :: Text.Text -> [Text.Text] -> [Statement] -> Parser Statement
asFunc name args = asLeave (SFuncDef name args)

parseFunc :: Parser Statement
parseFunc = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ keyword "=^.w.^="
    name <- funName
    args <- funArgs
    return (Lexer.IndentMany Nothing (asFunc name args) statements)


{- Return -}

parseReturn :: Parser Statement
parseReturn = lexeme $ do
    whitespace
    void $ keyword "bring"
    SReturn <$> parseExpr'

{- Continue -}
parseContinue :: Parser Statement
parseContinue = lexeme $ do
    whitespace
    void $ keyword "rest"
    return SContinue

{- Break -}

parseBreak :: Parser Statement
parseBreak = lexeme $ do
    whitespace
    void $ keyword "run away"
    return SBreak
