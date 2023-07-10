{-# LANGUAGE OverloadedStrings #-}

 module Meowscript.Parser.Core
 ( Parser
 , lexeme
 , whitespace
 , lexemeBi
 , lexemeBi'
 , lexemeLn
 , lexemeLnBi
 , whitespaceLn
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
import Meowscript.Parser.Keywords
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
blockComment = Lexer.skipBlockComment "~( ^.x.^)>" "<(^.x.^ )~" 

spaceChars :: Parser ()
spaceChars = (void . Mega.some . Mega.choice) (MChar.string <$> [ " ", "\t", "\\\n" ])

whitespace :: Parser ()
whitespace = Lexer.space spaceChars lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

lexemeBi :: Parser a -> Parser a
lexemeBi a = whitespace >> lexeme a

lexemeBi' :: Parser a -> Parser a
lexemeBi' a = whitespaceLn >> lexeme a

lexemeLn :: Parser a -> Parser a
lexemeLn = Lexer.lexeme whitespaceLn

lexemeLnBi :: Parser a -> Parser a
lexemeLnBi a = whitespaceLn >> lexemeLn a

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
keyword k = lexeme . (<?> "keyword") $ do
    (void . MChar.string) k
    Mega.notFollowedBy (Mega.satisfy validKeyChar)

tryKeyword :: Text.Text -> Parser ()
tryKeyword = Mega.try . keyword

validKeyChar :: Char -> Bool
validKeyChar c = isAlphaNum c || c `elem` ['\'', '_']

parseKey :: Parser Prim
parseKey = MeowKey <$> (parseKeyNew <|> parseKey')

parseKeyNew :: Parser KeyType
parseKeyNew = (Mega.try . keyword) meowLocal >> KeyNew <$> keyText

parseKey' :: Parser KeyType
parseKey' = KeyModify <$> keyText
    
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
    , parseFloat        <?> "float"
    , parseInt          <?> "int"
    , parseKey          <?> "key"   ]

parseStr :: Parser Prim
parseStr = do
    let quotation = ((<?> "quotation mark") . MChar.char) '"'
    void quotation
    (MeowString . Text.pack <$> Mega.many escapeStr) <* void quotation

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
parseFloat = Mega.try $ do
    let readDouble = Lexer.float
    MeowDouble <$> Lexer.signed whitespace readDouble

parseBool :: Parser Prim
parseBool = MeowBool <$> Mega.choice
    [ True  <$ keyword "happy"
    , False <$ keyword "sad" ]

parseLonely :: Parser Prim
parseLonely = MeowLonely <$ keyword "lonely"
