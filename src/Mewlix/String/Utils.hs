{-# LANGUAGE OverloadedStrings #-}

module Mewlix.String.Utils
( surround
, parens
, quotes
, brackets
, sepComma
, newline
, sepLines
, semicolon
, terminate
) where

import Data.Text (Text)
import qualified Data.Text as Text

surround :: Char -> Char -> Text -> Text
surround a b = Text.cons a . flip Text.snoc b

parens :: Text -> Text
parens = surround '(' ')'

quotes :: Text -> Text
quotes = surround '"' '"'

brackets :: Text -> Text
brackets = surround '[' ']'

sepComma :: [Text] -> Text
sepComma = Text.intercalate ", "

newline :: Text
newline = "\n"

sepLines :: [Text] -> Text
sepLines = Text.concat . map (<> newline)

semicolon :: Text
semicolon = ";"

terminate :: Text -> Text
terminate = (<> semicolon)
