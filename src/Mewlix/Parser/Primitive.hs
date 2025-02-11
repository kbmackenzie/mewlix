{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Primitive
( primitive
, home
, outside
, parseKey
, parseParams
) where

import Mewlix.Abstract.AST (Primitive(..), Params(..))
import Mewlix.Parser.Type (Parser, asks)
import Mewlix.Parser.Nesting (NestingFlag(..), nested, )
import Mewlix.Parser.Utils (lexeme, isKeyChar, whitespace, parensList)
import Mewlix.Parser.String (parseString, parseStringM)
import Mewlix.Parser.Keyword (keyword)
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Keywords.Types (SimpleKeyword(..))
import qualified Mewlix.Keywords.Constants as Keywords
import Mewlix.Keywords.Shadow (isShadowed)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec ((<?>))
import qualified Data.HashSet as HashSet
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (when, unless)

{- Prims: -}
----------------------------------------------------------------
primitive :: Parser Primitive
primitive = Mega.choice
    [ MewlixString  <$> parseStringM
    , MewlixString  <$> parseString
    , MewlixFloat   <$> parseFloat  <?> "number"
    , MewlixInt     <$> parseInt    <?> "number"
    , MewlixBool    <$> parseBool   <?> "boolean"
    , MewlixNil     <$  keyword Keywords.nil  <?> "nothing"
    , home 
    , outside                                               ]

{- Numbers and constants: -}
----------------------------------------------------------------
parseInt :: Parser Int
parseInt = lexeme (Lexer.signed whitespace Lexer.decimal)

parseFloat :: Parser Double
parseFloat = (Mega.try . lexeme) (Lexer.signed whitespace Lexer.float)

parseBool :: Parser Bool
parseBool = Mega.choice
    [ True  <$ keyword Keywords.true
    , False <$ keyword Keywords.false ]

{- Keys + identifers: -}
----------------------------------------------------------------
parseKeyText :: Parser Text
parseKeyText = lexeme (Mega.takeWhile1P (Just "key") isKeyChar)

isReserved :: Text -> Bool
isReserved key = isShadowed key || isReservedKeyword key
    where isReservedKeyword = (`HashSet.member` Keywords.reserved) . SimpleKeyword

{- Parse identifiers (variable names, function names, et cetera).
 - These cannot be reserved keywords. -}
parseKey :: Parser Key
parseKey = do
    text <- parseKeyText
    when (isReserved text)
        (fail (Text.unpack text ++ " is a reserved keyword!"))
    return (Key text)

parseParams :: Parser Params
parseParams = Params <$> parensList parseKey

{- Clowders: -}
----------------------------------------------------------------
home :: Parser Primitive
home = do
    keyword Keywords.home
    inClass <- asks (nested InClass)
    unless inClass $ do
        let word = (show . unwrapKeyword) Keywords.home
        fail . concat $ ["Cannot use keyword ", word, " outside clowder!"]
    return MewlixHome

outside :: Parser Primitive
outside = do
    keyword Keywords.outside
    inClass <- asks (nested InClass)
    unless inClass $ do
        let word = (show . unwrapKeyword) Keywords.outside
        fail . concat $ ["Cannot use keyword ", word, " outside clowder!"]
    return MewlixOutside
