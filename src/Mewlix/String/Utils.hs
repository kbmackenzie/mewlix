{-# LANGUAGE OverloadedStrings #-}

module Mewlix.String.Utils
( surround
, parens
, quotes
, brackets
, braces
, sepComma
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

braces :: Text -> Text
braces = surround '{' '}'

sepComma :: [Text] -> Text
sepComma = Text.intercalate ", "
