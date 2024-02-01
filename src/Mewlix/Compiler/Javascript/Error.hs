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
errorMessage pos message = mconcat
    [ message
    , "\n -> In module "
    , (quotes . escapeString . Text.pack . sourceName) pos
    , ", at line "
    , (showT . unPos . sourceLine) pos
    ]

createError :: SourcePos -> Text -> Text
createError pos message = do
    let arguments = mconcat [ "(" , errorCode CatOnComputer , "," , errorMessage pos message , ")" ]
    "throw new " <> mewlixError <> arguments
