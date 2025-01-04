{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Keywords.Shadow
( prefix
, shadow
) where

import Data.Text (Text)

prefix :: Text
prefix = "____"

shadow :: Text -> Text
shadow = mappend prefix
