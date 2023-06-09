{-# LANGUAGE OverloadedStrings #-}

module Meowscript.API.Args
( MeowrArg(..)
) where

import Meowscript.Core.AST
import Meowscript.Utils.IO
import Meowscript.Parser.Core (Parser)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Char (isSpace)

data MeowrArg =
      MeowrString Text.Text
    | MeowrFlag Text.Text
    | MeowrOption Text.Text [Text.Text]
    deriving (Show)

instance Eq MeowrArg where
    MeowrString _   == MeowrString _   = True
    MeowrFlag   _   == MeowrFlag   _   = True
    MeowrOption _ _ == MeowrOption _ _ = True
    _ == _ = False

whitespace :: Parser ()
whitespace = Lexer.space MChar.space1 Mega.empty Mega.empty 

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

quotes :: Parser a -> Parser a
quotes = Mega.between (MChar.char '"') (MChar.char '"')

parseString :: Parser Text.Text
parseString = Mega.choice
    [ parseQuoted
    , parseWord ]

parseWord :: Parser Text.Text
parseWord = lexeme $ Mega.takeWhile1P (Just "word") (not . isSpace)

parseQuoted :: Parser Text.Text
parseQuoted = (lexeme . quotes) $ Mega.takeWhile1P (Just "quoted") (/= '"')

parseFlag :: Parser Text.Text
parseFlag = lexeme (MChar.string "--" >> parseWord)
