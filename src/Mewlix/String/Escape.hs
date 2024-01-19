{-# LANGUAGE OverloadedStrings #-}

module Mewlix.String.Escape
( escapeString
, EscapeFunc
, escapeStringF
) where

import Data.Text (Text)
import qualified Data.Text as Text

{- Escape Sequences-}
----------------------------------------------------------------------------------------
-- The following escape sequences are meant to be valid in both Mewlix *and* Javascript.
escapeCharacter :: Char -> Text
escapeCharacter c = case c of
    '\n'    -> "\\n"
    '\t'    -> "\\t"
    '\r'    -> "\\r"
    '\b'    -> "\\b"
    '\f'    -> "\\f"
    '\v'    -> "\\v"
    '/'     -> "\\/"
    '''     -> "\\'"
    '"'     -> "\\\""
    '\\'    -> "\\\\"
    other   -> Text.singleton other

escapeString :: Text -> Text
escapeString = Text.concatMap escapeCharacter

{- Alternative Escaping -}
----------------------------------------------------------------------------------------
type EscapeFunc = Char -> Text

escapeStringF :: EscapeFunc -> Text -> Text
escapeStringF = Text.concatMap
