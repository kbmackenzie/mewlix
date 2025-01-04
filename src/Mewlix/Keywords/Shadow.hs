{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Keywords.Shadow
( prefix
, shadow
, isShadowed
) where

import Data.Text (Text)
import qualified Data.Text as Text

prefix :: Text
prefix = "____"

shadow :: Text -> Text
shadow = Text.append prefix

isShadowed :: Text -> Bool
isShadowed = Text.isPrefixOf prefix
