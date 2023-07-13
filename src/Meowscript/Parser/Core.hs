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
 , sepByComma'
 , keyword
 , tryKeyword
 , specialSymbol
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
import Data.Bifunctor (bimap)

type Parser = Mega.Parsec Void Text.Text

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "--"

{- I'm commenting this out because I have to write my own block comment parser in order to
 - make it not case-sensitive.
 - (The default implementation uses 'string' and not the case-insensitive 'string'' variant.
 -
blockComment :: Parser ()
blockComment = Lexer.skipBlockComment "~( ^.x.^)>" "<(^.x.^ )~" 
-}

blockComment :: Parser ()
blockComment = start >> void (Mega.manyTill Mega.anySingle end)
    where (start, end) = bimap MChar.string' MChar.string' meowComment

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

comma :: Parser Char
comma = MChar.char ','

commaLn :: Parser Char
commaLn = lexemeLn comma

sepByComma :: Parser a -> Parser [a]
sepByComma = flip Mega.sepBy comma

sepByComma' :: Parser a -> Parser [a]
sepByComma' = flip Mega.sepEndBy commaLn

keyword :: Text.Text -> Parser ()
keyword k = lexeme . (<?> "keyword") $ do
    (void . MChar.string) k
    Mega.notFollowedBy (Mega.satisfy validKeyChar)

tryKeyword :: Text.Text -> Parser ()
tryKeyword = Mega.try . keyword

specialSymbol :: Text.Text -> Parser ()
specialSymbol = lexeme . void . MChar.string'

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
