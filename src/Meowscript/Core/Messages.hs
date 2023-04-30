{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Messages
( showT
, unopError
, binopError
) where

import Meowscript.Core.AST
import Meowscript.Core.Exceptions
import qualified Data.Text as Text

showT :: (Show a) => a -> Text.Text
showT = Text.pack . show

unopError :: Text.Text -> Text.Text -> Prim -> Text.Text
unopError op symbol a = Text.intercalate " "
    [ "Invalid operation:", op, "=>", symbol, showT a ]

binopError :: Text.Text -> Text.Text -> Prim -> Prim -> Text.Text
binopError op symbol a b = Text.intercalate " "
    [ "Invalid operation:", op, "=>", showT a, symbol, showT b ]
