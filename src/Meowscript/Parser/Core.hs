{-# LANGUAGE OverloadedStrings #-}

 module Meowscript.Parser.Core
 ( Parser
 , indented
 , notIndented
 , lexemeLn
 , whitespaceLn
 , lexeme
 , whitespace
 , symbol
 , trySymbol
 , parens
 , quotes
 , bars
 , keyword
 , meowDiv
 , stmtEnd
 , parseStr
 , validKeyChar
 , parseKey
 , keyText
 , parseInt
 , parseFloat
 , parseBool
 , parseLonely
 , parsePrim
 ) where

import Meowscript.Core.AST
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Void (Void)
import Control.Monad (void)
import Data.Char (isAlphaNum)

type Parser = Mega.Parsec Void Text.Text

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "--"

blockComment :: Parser ()
blockComment = Lexer.skipBlockComment "<(=^.x.^= )~" "~( =^.x.^=)>"

notIndented :: Parser a -> Parser a
notIndented = Lexer.nonIndented whitespaceLn

indented :: Parser (Lexer.IndentOpt Parser a b) -> Parser a
indented = Lexer.indentBlock whitespaceLn

whitespaceLn :: Parser ()
whitespaceLn = Lexer.space MChar.space1 lineComment blockComment

-- Like 'whitespaceLn', but doesn't skip newlines.
whitespace :: Parser ()
whitespace = Lexer.space (void $ Mega.some (MChar.char ' ' <|> MChar.char '\t')) lineComment blockComment

lexemeLn :: Parser a -> Parser a
lexemeLn = Lexer.lexeme whitespaceLn

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

symbol :: Text.Text -> Parser Text.Text
symbol = Lexer.symbol whitespace

trySymbol :: Text.Text -> Parser Text.Text
trySymbol s = lexeme $ Mega.try $ do
    MChar.string s

parens :: Parser a -> Parser a
parens = Mega.between (MChar.char '(') (MChar.char ')')

quotes :: Parser a -> Parser a
quotes = Mega.between (MChar.char '"') (MChar.char '"')

bars :: Parser a -> Parser a
bars = Mega.between (MChar.char '|') (MChar.char '|')

keyword :: Text.Text -> Parser ()
keyword k = lexeme $ do
    void $ MChar.string k
    Mega.notFollowedBy $ Mega.satisfy validKeyChar

meowDiv :: Text.Text -> Parser a -> Parser a
meowDiv k = Mega.between (MChar.string k) (MChar.string "leave")

stmtEnd :: Parser Char
stmtEnd = lexeme (MChar.char ';' <|> MChar.char '\n')

reservedKeywords :: [Text.Text]
reservedKeywords =
    [ "purr"
    , "paw"
    , "hiss!"
    , "mew?"
    , "scratch"
    , "leave"
    , "bring"
    , "rest"
    , "run"
    , "away"
    , "poke"
    , "nudge"
    , "peek"
    , "sneak"
    , "push"
    , "bap"
    , "knock"
    , "over"
    , "lonely"
    , "yummy"
    , "icky" ]

validKeyChar :: Char -> Bool
validKeyChar c = isAlphaNum c || c `elem` ['\'', '^', '_']

parseKey :: Parser Prim
parseKey = MeowKey <$> keyText
    
keyText :: Parser Text.Text
keyText = do
    x <- Mega.takeWhile1P (Just "key") validKeyChar
    if x `elem` reservedKeywords
    then fail "Variable name cannot be a keyword!"
    else return x

parsePrim :: Parser Prim
parsePrim = Mega.choice
    [ parseStr <?> "string"
    , parseBool <?> "bool"
    , parseLonely <?> "lonely"
    , Mega.try parseInt <?> "int"
    , parseFloat <?> "float"
    , parseKey <?> "key" ]

parseStr :: Parser Prim
parseStr = do
    void $ MChar.char '"'
    x <- Text.pack <$> Mega.many escapeStr 
    void $ MChar.char '"'
    (return . MeowString) x

escapeStr :: Parser Char
escapeStr = Mega.choice
    [ '"' <$ MChar.string "\\\""
    , '/' <$ MChar.string "\\/"
    , '\b' <$ MChar.string "\\b"
    , '\f' <$ MChar.string "\\f"
    , '\n' <$ MChar.string "\\n"
    , '\r' <$ MChar.string "\\r"
    , '\t' <$ MChar.string "\\t"
    , '\\' <$ MChar.string "\\\\"
    , Mega.satisfy (/= '"') ]

parseInt :: Parser Prim
parseInt = do
    let readInt = Lexer.decimal
    x <- Lexer.signed whitespace readInt
    (return . MeowInt) x

parseFloat :: Parser Prim
parseFloat = do
    let readDouble = Lexer.float
    x <- Lexer.signed whitespace readDouble
    (return . MeowDouble) x

parseBool :: Parser Prim
parseBool =  do
    x <- Mega.choice
        [ True <$ keyword "yummy"
        , False <$ keyword "icky" ]
    (return . MeowBool) x

parseLonely :: Parser Prim
parseLonely = keyword "lonely" >> return MeowLonely
