module Mewlix.REPL.Parse
( parseLine
) where

import Mewlix.REPL.Actions
import Data.Text (Text)
import Mewlix.Parser.Expr (liftedExpr)
import Mewlix.Parser.Utils (Parser)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)
import Data.Char (isSpace)

parseLine :: Parser Line
parseLine = Mega.choice
    [ Meta       <$> parseCommand
    , Expression <$> lexeme liftedExpr ]

parseCommand :: Parser LineCommand
parseCommand = do
    (void . lexeme . MChar.char) ':'
    command <- parseWord
    args <- Mega.many parseString
    return (LineCommand command args)

whitespace :: Parser ()
whitespace = Lexer.space MChar.space1 Mega.empty Mega.empty 

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

quotes :: Parser a -> Parser a
quotes = Mega.between (MChar.char '"') (MChar.char '"')

parseString :: Parser Text
parseString = Mega.choice
    [ parseQuoted
    , parseWord ]

parseWord :: Parser Text
parseWord = lexeme $ Mega.takeWhile1P (Just "word") (not . isSpace)

parseQuoted :: Parser Text
parseQuoted = (lexeme . quotes) $ Mega.takeWhile1P (Just "quoted") (/= '"')
