{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Parser.Prim
( parsePrim
, parseKey
, parseStr
) where

import Meowscript.Parser.AST
import Meowscript.Parser.Utils
import Meowscript.Parser.Keywords
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
