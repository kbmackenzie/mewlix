{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Javascript.Statement
( terminate
) where

import Data.Text (Text)
import Mewlix.String.Utils ()

semicolon :: Text
semicolon = ";"

terminate :: Text -> Text
terminate = (<> semicolon)
