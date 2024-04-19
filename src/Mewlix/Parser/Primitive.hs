{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Primitive
( parsePrim
, parseString
, parseStringM
, parseKeyText
, parseKey
, parseParams
) where

import Mewlix.Abstract.AST (Primitive(..), Params(..))
import Mewlix.Parser.Type (Parser)
import Mewlix.Parser.Utils (lexeme, isKeyChar, whitespace, parensList)
import Mewlix.Parser.String (parseString, parseStringM)
import Mewlix.Parser.Keyword (keyword)
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Keywords.Types (SimpleKeyword(..))
import qualified Mewlix.Keywords.LanguageKeywords as Keywords
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec ((<?>))
import qualified Data.HashSet as HashSet
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (when)

{- Prims: -}
----------------------------------------------------------------
parsePrim :: Parser Primitive
parsePrim = Mega.choice
    [ MewlixString  <$> parseStringM
    , MewlixString  <$> parseString
    , MewlixFloat   <$> parseFloat
    , MewlixInt     <$> parseInt
    , MewlixBool    <$> parseBool
    , MewlixNil     <$  keyword Keywords.nil
    , MewlixHome    <$  keyword Keywords.home ]

{- Numbers and constants: -}
----------------------------------------------------------------
parseInt :: Parser Int
parseInt = lexeme (Lexer.signed whitespace Lexer.decimal) <?> "int"

parseFloat :: Parser Double
parseFloat = Mega.try (lexeme (Lexer.signed whitespace Lexer.float)) <?> "float"

parseBool :: Parser Bool
parseBool = Mega.choice
    [ True  <$ keyword Keywords.true
    , False <$ keyword Keywords.false ] <?> "boolean"

{- Keys + identifers: -}
----------------------------------------------------------------
parseKeyText :: Parser Text
parseKeyText = lexeme (Mega.takeWhile1P (Just "key") isKeyChar)

{- Parse identifiers (variable names, function names, et cetera).
 - These cannot be reserved keywords. -}
parseKey :: Parser Key
parseKey = do
    text <- parseKeyText
    when (HashSet.member (SimpleKeyword text) Keywords.reserved) 
        (fail (Text.unpack text ++ " is a reserved keyword!"))
    return (Key text)

parseParams :: Parser Params
parseParams = Params <$> parensList parseKey
