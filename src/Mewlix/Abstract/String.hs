{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Abstract.String
( surround
, escapeString
) where

import Data.Text (Text)
import qualified Data.Text as Text

{- Utils -}
---------------------------------------------------------------
surround :: Char -> Text -> Text
surround c str = Text.concat
    [ Text.singleton c, str, Text.singleton c ]

{- Transpilation -}
---------------------------------------------------------------
escapeCharacter :: Char -> Text
escapeCharacter c = case c of
    '\n'    -> "\\n"
    '\t'    -> "\\t"
    '\r'    -> "\\r"
    '\b'    -> "\\b"
    '\f'    -> "\\f"
    '/'     -> "\\/"
    '"'     -> "\\\""
    '\\'    -> "\\\\"
    other   -> Text.singleton other

escapeString :: Text -> Text
escapeString = Text.concatMap escapeCharacter
