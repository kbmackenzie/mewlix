{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Compiler.Indentation
( Indentation(..)
, createIndent
, indentLine
, indentMany
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List

newtype Indentation = Indentation { getIndent :: Int }
    deriving (Eq, Ord, Show, Enum, Num)

createIndent :: Indentation -> Text
createIndent = Text.pack . flip List.replicate ' ' . getIndent

indentLine :: Indentation -> Text -> Text
indentLine = flip mappend . createIndent

indentMany :: Indentation -> [Text] -> [Text]
indentMany = map . indentLine
