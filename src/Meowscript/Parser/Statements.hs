{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Statements
( root
, statements
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Meowscript.Parser.Expr
import Meowscript.Parser.Keywords
import Text.Megaparsec ((<?>))
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)

root :: Parser Statement
root = SAll <$> Mega.between whitespaceLn Mega.eof (Mega.many (lexemeLn statements))

statements :: Parser Statement
statements = Mega.choice
    [ Mega.try parseIf          <?> "if"
    , Mega.try parseWhile       <?> "while"
    , Mega.try parseIfElse      <?> "if/else"
    , Mega.try parseFunc        <?> "function"
    , Mega.try parseReturn      <?> "return"
    , Mega.try parseContinue    <?> "continue"
    , Mega.try parseBreak       <?> "break"
    , exprS                     <?> "expression"
    , fail "Invalid token!" ]

asEnd :: ([Statement] -> Statement) -> [Statement] -> Parser Statement
asEnd f xs = do
    whitespace
    (void . keyword) meowEnd
    return (f xs)

condition :: Parser Expr
condition = (lexeme . bars) parseExpr'

exprS :: Parser Statement
exprS = whitespace >> SExpr <$> parseExpr'


{- If -}

asIf :: Expr -> [Statement] -> Parser Statement
asIf e = asEnd (SOnlyIf e)

parseIf :: Parser Statement
parseIf = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ keyword meowIf
    whitespace
    c <- condition
    return (Lexer.IndentMany Nothing (asIf c) statements)


{- While -}

asWhile :: Expr -> [Statement] -> Parser Statement
asWhile e = asEnd (SWhile e)

parseWhile:: Parser Statement
parseWhile = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ keyword meowWhile
    whitespace
    c <- condition
    return (Lexer.IndentMany Nothing (asWhile c) statements)



{- If Else -}

binAsIf :: Expr -> [Statement] -> Parser ([Statement] -> Statement)
binAsIf e xs = return $ SIfElse e xs

binAsElse :: ([Statement] -> Statement) -> [Statement] -> Parser Statement
binAsElse f xs = do
    void $ keyword meowEnd
    return (f xs)

parseIfElse :: Parser Statement
parseIfElse = do
    x <- parseBinIf
    parseBinElse x

parseBinIf :: Parser ([Statement] -> Statement)
parseBinIf = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ keyword meowIf
    whitespace
    c <- condition
    return (Lexer.IndentMany Nothing (binAsIf c) statements)

parseBinElse :: ([Statement] -> Statement) -> Parser Statement
parseBinElse f = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ keyword meowElse
    return (Lexer.IndentMany Nothing (binAsElse f) statements)


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
asFunc name args = asEnd (SFuncDef name args)

parseFunc :: Parser Statement
parseFunc = lexeme $ Lexer.indentBlock whitespaceLn $ do
    void $ keyword meowCatface
    name <- funName
    args <- funArgs
    return (Lexer.IndentMany Nothing (asFunc name args) statements)


{- Return -}

parseReturn :: Parser Statement
parseReturn = lexeme $ do
    whitespace
    void $ keyword meowReturn
    SReturn <$> parseExpr'

{- Continue -}
parseContinue :: Parser Statement
parseContinue = lexeme $ do
    whitespace
    void $ keyword meowContinue
    return SContinue

{- Break -}

parseBreak :: Parser Statement
parseBreak = lexeme $ do
    whitespace
    void $ keyword meowBreak
    return SBreak
