{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Utils.Text
( (|++)
, parens
) where

import Data.Text (Text)
import qualified Data.Text as Text

-- Util operator for joining text together.
(|++) :: Text -> Text -> Text
(|++) = Text.append

parens :: Text -> Text
parens x = Text.concat [ "(", x, ")" ]

