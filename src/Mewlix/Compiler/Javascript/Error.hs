{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Javascript.Error
( createError
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.String.Utils (quotes)
import Mewlix.String.Escape (escapeString)
import Mewlix.Compiler.Javascript.Constants (mewlix)
import Mewlix.Utils.Show (showT)
import Text.Megaparsec.Pos (SourcePos(..), unPos)

data ErrorCode =
      TypeMisMatch
    | InvalidOp
    | DivideByZero
    | InvalidImport
    | BadConversion
    | CatOnComputer
    | CriticalError
    | ExternalError
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

mewlixError :: Text
mewlixError = mewlix "MewlixError"

errorCode :: ErrorCode -> Text
errorCode = (mewlix "ErrorCode." <>) . showT

errorMessage :: SourcePos -> Text -> Text
errorMessage pos info = mconcat
    [ info
    , "\n -> In module "
    , (quotes . escapeString . Text.pack . sourceName) pos
    , ", at line "
    , (showT . unPos . sourceLine) pos
    ]

errorArgs :: ErrorCode -> SourcePos -> Text -> Text
errorArgs code pos info = mconcat
    [ "(" 
    , errorCode code 
    , ","
    , errorMessage pos info
    , ")" ]

createError :: ErrorCode -> SourcePos -> Text -> Text
createError code pos info = "throw new " <> mewlixError <> errorArgs code pos info
