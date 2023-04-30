{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Exceptions
( showException
) where

import qualified Data.Text as Text

data MeowException =
      MeowBadVar
    | MeowInvalidOp
    | MeowStackOverflow
    deriving (Eq, Ord)

instance Show MeowException where
    show MeowBadVar = "BadVariableException"
    show MeowInvalidOp = "InvalidOperationException"
    show MeowStackOverflow = "StackOverflowException"

showException :: MeowException -> Text.Text -> Text.Text
showException meowe message = Text.concat [(Text.pack . show) meowe, ": ", message]
