{-# LANGUAGE OverloadedStrings #-}

module Mewlix.REPL.Parse
( parseLine 
) where

import Mewlix.REPL.Core
import Data.Text (Text)
import Mewlix.Parser.Expr (liftedExpr)
import Mewlix.Parser.Utils (Parser)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)
import Data.Char (isSpace)

{- Parser Utils -}
----------------------------------------------------------------
whitespace :: Parser ()
whitespace = Lexer.space MChar.space1 Mega.empty Mega.empty 

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

quotes :: Parser a -> Parser a
quotes = Mega.between (MChar.char '"') (MChar.char '"')

keyword :: Text -> Parser ()
keyword = void . lexeme . MChar.string

symbol :: Char -> Parser ()
symbol = void . lexeme . MChar.char

parseString :: Parser Text
parseString = Mega.choice
    [ parseQuoted
    , parseWord  ]

parseWord :: Parser Text
parseWord = lexeme $ Mega.takeWhile1P (Just "word") (not . isSpace)

parseQuoted :: Parser Text
parseQuoted = (lexeme . quotes) $ Mega.takeWhile1P (Just "quoted") (/= '"')

{- Actions -}
----------------------------------------------------------------
actions :: [(Text, Action)]
actions =
    [ ("help"       , Help      )
    , ("load"       , Load      )
    , ("quit"       , Quit      )
    , ("inspect"    , Inspect   )
    , ("ask"        , Ask       )
    , ("clear"      , Clear     ) ]

parseAction :: Parser Action
parseAction = do
    let parse :: (Text, Action) -> Parser Action
        parse (key, act) = act <$ keyword key
    Mega.choice (map parse actions)

parseCommand :: Parser LineCommand
parseCommand = do
    symbol ':'
    action <- parseAction
    args   <- Mega.many parseString
    return $ LineCommand action args

{- Line -}
----------------------------------------------------------------
parseLine :: Parser Line
parseLine = Mega.choice
    [ Meta       <$> parseCommand
    , Expression <$> lexeme liftedExpr ]
