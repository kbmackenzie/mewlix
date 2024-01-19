{-# LANGUAGE OverloadedStrings #-}

module Mewlix.String.Escape
( escapeString
) where

import Data.Text (Text)
import qualified Data.Text as Text

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
