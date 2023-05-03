{-# LANGUAGE OverloadedStrings #-}

 module Meowscript.Parser.Core
 ( Parser
 , lexeme
 , whitespace
 , flexeme
 , lexemeLn
 , whitespaceLn
 , lnLexeme
 , symbol
 , trySymbol
 , parens
 , quotes
 , brackets
 , sepByComma
 , keyword
 , tryKeyword
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

spaceChars :: Parser ()
spaceChars = (void . Mega.some . Mega.choice) (MChar.char <$> [ ' ', '\t' ])

whitespace :: Parser ()
whitespace = Lexer.space spaceChars lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

flexeme :: Parser a -> Parser a
flexeme a = whitespace >> lexeme a

lexemeLn :: Parser a -> Parser a
lexemeLn = Lexer.lexeme whitespaceLn

lnLexeme :: Parser a -> Parser a
lnLexeme a = whitespaceLn >> lexeme a

whitespaceLn :: Parser ()
whitespaceLn = Lexer.space MChar.space1 lineComment blockComment

symbol :: Text.Text -> Parser Text.Text
symbol = Lexer.symbol whitespace

trySymbol :: Text.Text -> Parser Text.Text
trySymbol = lexeme . Mega.try . symbol

parens :: Parser a -> Parser a
parens = Mega.between (MChar.char '(') (MChar.char ')')

quotes :: Parser a -> Parser a
quotes = Mega.between (MChar.char '"') (MChar.char '"')

brackets :: Parser a -> Parser a
brackets = Mega.between (MChar.char '[') (MChar.char ']')

sepByComma :: Parser a -> Parser [a]
sepByComma x = Mega.sepBy x (MChar.char ',')

keyword :: Text.Text -> Parser ()
keyword k = lexeme $ do
    (void . MChar.string) k
    Mega.notFollowedBy (Mega.satisfy validKeyChar)

tryKeyword :: Text.Text -> Parser ()
tryKeyword = Mega.try . keyword

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
    [ parseStr          <?> "string"
    , parseBool         <?> "bool"
    , parseLonely       <?> "lonely"
    , Mega.try parseInt <?> "int"
    , parseFloat        <?> "float"
    , parseKey          <?> "key"   ]

parseStr :: Parser Prim
parseStr = do
    (void . MChar.char) '"'
    (MeowString . Text.pack <$> Mega.many escapeStr) <* (void . MChar.char) '"'

escapeStr :: Parser Char
escapeStr = Mega.choice
    [ '"'  <$ MChar.string "\\\""
    , '/'  <$ MChar.string "\\/"
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
    MeowInt <$> Lexer.signed whitespace readInt

parseFloat :: Parser Prim
parseFloat = do
    let readDouble = Lexer.float
    MeowDouble <$> Lexer.signed whitespace readDouble

parseBool :: Parser Prim
parseBool = MeowBool <$> Mega.choice
    [ True <$ keyword "yummy"
    , False <$ keyword "icky" ]

parseLonely :: Parser Prim
parseLonely = MeowLonely <$ keyword "lonely"
