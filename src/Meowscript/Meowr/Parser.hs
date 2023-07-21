{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Meowr.Parser
( parseMeowed
) where

import Meowscript.Meowr.Core
import Meowscript.Parser.Core (Parser)
import qualified Data.Text as Text
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Applicative (liftA2)
import Data.Char (isAlphaNum)

parseMeowed :: Text.Text -> Either Text.Text MeowrAction
parseMeowed str = case Mega.parse parseAction "<args>" str of
    (Left x) -> (Left . Text.pack . errorBundlePretty) x
    (Right x) -> Right x

parseAction :: Parser MeowrAction
parseAction = do
    name    <- Mega.optional parseString
    args    <- Mega.many parseArg
    return (MeowrAction name args)

parseArg :: Parser MeowrArg
parseArg = Mega.choice
    [ Mega.try parseOption          <?> "option"
    , parseFlag                     <?> "flag"
    , MeowrString <$> parseString   <?> "string" ]

whitespace :: Parser ()
whitespace = Lexer.space MChar.space1 Mega.empty Mega.empty 

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

quotes :: Parser a -> Parser a
quotes = Mega.between (MChar.char '"') (MChar.char '"')

lexChar :: Char -> Parser Char
lexChar = lexeme . MChar.char

lexChar' :: Char -> Parser (Maybe Char)
lexChar' = Mega.optional . lexChar

parseString :: Parser Text.Text
parseString = Mega.choice
    [ parseQuoted
    , parseWord ]

validChars :: [Char]
validChars = [ '_', '.', '\\', '/' ]

parseWord :: Parser Text.Text
parseWord = lexeme $ Mega.takeWhile1P (Just "word") predicate
    where predicate = liftA2 (||) isAlphaNum (`elem` validChars)

parseQuoted :: Parser Text.Text
parseQuoted = (lexeme . quotes) $ Mega.takeWhile1P (Just "quoted") (/= '"')

parseFlag :: Parser MeowrArg
parseFlag = MeowrFlag <$> (lexChar '-' >> parseWord)

parseOption :: Parser MeowrArg
parseOption = MeowrOption <$> (lexChar '-' >> parseWord) <*> (lexChar' '=' >> parseString)
