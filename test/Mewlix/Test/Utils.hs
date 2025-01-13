{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Test.Utils
( generateDescription
) where

import Data.Text (Text)
import qualified Data.Text as Text

generateDescription :: String -> Text -> String
generateDescription intro input = do
    let snippet = if Text.length input >= 50
        then Text.take 50 input `Text.append` "..."
        else input
    intro ++ show snippet
