{-# LANGUAGE OverloadedStrings #-}

module Mewlix.String.Utils
( (|++)
, parens
, quotes
, surround
) where

import Data.Text (Text)
import qualified Data.Text as Text

-- Util operator for joining text together.
(|++) :: Text -> Text -> Text
(|++) = Text.append

parens :: Text -> Text
parens x = Text.concat [ "(", x, ")" ]

quotes :: Text -> Text
quotes x = Text.concat [ "\"", x, "\"" ]

surround :: Char -> Text -> Text
surround c str = Text.concat
    [ Text.singleton c, str, Text.singleton c ]
