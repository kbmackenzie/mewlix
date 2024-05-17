{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.Error
( ErrorCode(..)
, errorInfo
, createError
) where

import Data.Text (Text)
import Mewlix.String.Escape (escapeString)
import Mewlix.String.Utils (quotes, parens)
import Mewlix.Compiler.JavaScript.Constants (mewlix')
import Mewlix.Utils.Show (showT)
import Text.Megaparsec.Pos (SourcePos(..), unPos)

data ErrorCode =
      TypeMisMatch
    | InvalidOperation
    | InvalidConversion
    | CatOnComputer
    | Console
    | Graphic
    | InvalidImport
    | CriticalError
    | ExternalError
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

mewlixError :: Text
mewlixError = mewlix' "MewlixError"

errorCode :: ErrorCode -> Text
errorCode = (mewlix' "ErrorCode." <>) . showT

errorInfo :: SourcePos -> Text
errorInfo pos = (quotes . escapeString . mconcat)
    [ "\n -> In yarn ball "
    , (showT . sourceName) pos
    , ", at line "
    , (showT . unPos . sourceLine) pos ]     

createError :: ErrorCode -> SourcePos -> Text -> Text
createError code pos expr = do
    let message = parens expr <> " + " <> errorInfo pos
    let arguments = parens (errorCode code <> ", " <> message)
    parens ("new " <> mewlixError <> arguments)
