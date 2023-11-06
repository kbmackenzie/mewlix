{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Prim
( parsePrim
, parseString
, parseStringM
, parseKey
, parseName
) where

import Mewlix.Parser.AST
import Mewlix.Parser.Utils
import Mewlix.Parser.Keywords
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec ((<?>))
import qualified Data.HashSet as HashSet
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void, when)
import Data.Char (isAlphaNum, isAscii)

parsePrim :: Parser ParserPrim
parsePrim = Mega.choice
    [ PrimStr   <$> parseString
    , PrimStr   <$> parseStringM
    , PrimFloat <$> parseFloat
    , PrimInt   <$> parseInt
    , PrimBool  <$> parseBool
    , PrimNil   <$  parseNil     ]


-- Escape sequences.
escapeChar :: Char -> Char
escapeChar c = case c of
    'n'     -> '\n'
    't'     -> '\t'
    'b'     -> '\b'
    'r'     -> '\r'
    'f'     -> '\f'
    other   -> other


-- Single-line strings.
stringChar :: Parser Char
stringChar = Mega.choice
    [ MChar.char '\\' >> fmap escapeChar Mega.anySingle
    , MChar.newline >> fail "Linebreak in string!"
    , Mega.satisfy (/= '"')                             ]

parseString :: Parser Text
parseString = do
    let quotation :: Parser ()
        quotation = (void . MChar.char) '"' <?> "quotation mark"

    quotation
    text <- Text.pack <$> Mega.many stringChar <?> "string"
    lexeme quotation
    return text


-- Multinline strings.
stringCharM :: Parser Char
stringCharM = Mega.choice
    [ MChar.char '\\' >> fmap escapeChar Mega.anySingle
    , Mega.satisfy (/= '"')                             ]

parseStringM :: Parser Text
parseStringM = do
    let quotations :: Parser ()
        quotations = (void . MChar.string) "\"\"\"" <?> "triple quotes"

    quotations
    text <- Text.pack <$> Mega.many stringCharM <?> "multiline string"
    lexeme quotations
    return text

parseInt :: Parser Int
parseInt = lexeme (Lexer.signed whitespace Lexer.decimal) <?> "int"

parseFloat :: Parser Double
parseFloat = Mega.try (lexeme (Lexer.signed whitespace Lexer.float)) <?> "float"

parseBool :: Parser Bool
parseBool = Mega.choice
    [ True  <$ MChar.string meowTrue
    , False <$ MChar.string meowFalse ] <?> "boolean"

parseNil :: Parser ()
parseNil = (void . MChar.string) meowNil


-- Keys, keywords and identifiers:

{- Allow non-Ascii characters in keys.
 -
 - Language keywords are only gonna use AlphaNumeric characters.
 -
 - Allowing non-Ascii characters lets Unicode characters be identifiers,
 - whilst not conflicting with the language keywords, which only use Ascii characters. -}

isKeyChar :: Char -> Bool
isKeyChar c = isAlphaNum c || c == '_' || (not . isAscii) c

parseKey :: Parser Text
parseKey = lexeme (Mega.takeWhile1P (Just "key") isKeyChar)

{- Parse identifiers (variable names, function names, et cetera).
 - These cannot be reserved keywords. -}
parseName :: Parser Text
parseName = do
    text <- parseKey <?> "identifier"
    when (HashSet.member text reservedKeywords) 
        (fail (Text.unpack text ++ " is a reserved keyword!"))
    return text



{-
module Mewlix.Parser.Prim
( parsePrim
, parseKey
, parseStr
) where

import Mewlix.Parser.AST
import Mewlix.Parser.Utils
import Mewlix.Parser.Keywords
import qualified Data.Text as Text
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)

parsePrim :: Parser ParserPrim
parsePrim = Mega.choice
    [ parseStr          <?> "string"
    , parseStrM         <?> "string"
    , parseBool         <?> "bool"
    , parseLonely       <?> "nothing"
    , parseFloat        <?> "float"
    , parseInt          <?> "int"    ]

parseStr :: Parser ParserPrim
parseStr = do
    let quotation = ((<?> "quotation mark") . MChar.char) '"'
    void quotation
    (PrimStr . Text.pack <$> Mega.many escapeStr) <* void quotation

escapes :: [Parser Char]
escapes =
    [ '"'  <$ MChar.string "\\\""
    , '/'  <$ MChar.string "\\/"
    , '\b' <$ MChar.string "\\b"
    , '\f' <$ MChar.string "\\f"
    , '\n' <$ MChar.string "\\n"
    , '\r' <$ MChar.string "\\r"
    , '\t' <$ MChar.string "\\t"
    , '\\' <$ MChar.string "\\\\"
    , Mega.satisfy (/= '"') ]

-- Escapes single-line string.
escapeStr :: Parser Char
escapeStr = Mega.choice (newlineGuard : escapes)
    where newlineGuard = MChar.newline >> fail "Linebreak in string!"

-- Escapes multiline string.
escapeStrM :: Parser Char
escapeStrM = Mega.choice escapes

-- Parse multiline string.
parseStrM :: Parser ParserPrim
parseStrM = do
    let quotation = ((<?> "triple quotes") . MChar.string) "\"\"\""
    void quotation
    (PrimStr . Text.pack <$> Mega.many escapeStrM) <* void quotation

parseInt :: Parser ParserPrim
parseInt = do
    let readInt = Lexer.decimal
    PrimInt <$> Lexer.signed whitespace readInt

parseFloat :: Parser ParserPrim
parseFloat = Mega.try $ do
    let readDouble = Lexer.float
    PrimFloat <$> Lexer.signed whitespace readDouble

parseBool :: Parser ParserPrim
parseBool = PrimBool <$> Mega.choice
    [ True  <$ keyword meowTrue
    , False <$ keyword meowFalse ]

parseLonely :: Parser ParserPrim
parseLonely = PrimNil <$ keyword meowNil

parseKey :: Parser Expr
parseKey = ExprKey <$> keyText
-}
