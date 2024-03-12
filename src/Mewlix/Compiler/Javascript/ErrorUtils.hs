{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Javascript.ErrorUtils
( ErrorCode(..)
, errorInfo
, createError
, createErrorIIFE
) where

import Data.Text (Text)
import Mewlix.String.Utils (quotes)
import Mewlix.String.Escape (escapeString)
import Mewlix.Compiler.Javascript.Constants (mewlix)
import Mewlix.Utils.Show (showT)
import Text.Megaparsec.Pos (SourcePos(..), unPos)

data ErrorCode =
      TypeMisMatch
    | DivideByZero
    | BadConversion
    | CatOnComputer
    | Console
    | Graphic
    | InvalidImport
    | CriticalError
    | ExternalError
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

mewlixError :: Text
mewlixError = mewlix "MewlixError"

errorCode :: ErrorCode -> Text
errorCode = (mewlix "ErrorCode." <>) . showT

-- This returns a valid Javascript string.
errorInfo :: SourcePos -> Text
errorInfo pos = (quotes . escapeString . mconcat)
    [ "\n -> In module "
    , (showT . sourceName) pos
    , ", at line "
    , (showT . unPos . sourceLine) pos ]

errorArgs :: ErrorCode -> SourcePos -> Text -> Text
errorArgs code pos message = mconcat
    [ "(" , errorCode code , "," , message, " + ", errorInfo pos, ")" ]

createError :: ErrorCode -> SourcePos -> Text -> Text
createError code pos expr = "throw new " <> mewlixError <> errorArgs code pos expr

createErrorIIFE :: ErrorCode -> SourcePos -> Text -> Text
createErrorIIFE code pos expr = "(() => { " <> createError code pos expr <> " })()"
