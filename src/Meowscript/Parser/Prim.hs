{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Parser.Prim
( parsePrim
, parseKey
, parseStr
) where

import Meowscript.Parser.AST
import Meowscript.Parser.Utils
import qualified Data.Text as Text
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)

parsePrim :: Parser ParserPrim
parsePrim = Mega.choice
    [ parseStr          <?> "string"
    , parseBool         <?> "bool"
    , parseLonely       <?> "lonely"
    , parseFloat        <?> "float"
    , parseInt          <?> "int"    ]

parseStr :: Parser ParserPrim
parseStr = do
    let quotation = ((<?> "quotation mark") . MChar.char) '"'
    void quotation
    (PrimStr . Text.pack <$> Mega.many escapeStr) <* void quotation

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
    [ True  <$ keyword "happy"
    , False <$ keyword "sad" ]

parseLonely :: Parser ParserPrim
parseLonely = PrimNil <$ keyword "lonely"

parseKey :: Parser Expr
parseKey = ExprKey <$> keyText
