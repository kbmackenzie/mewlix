{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Compiler.Indentation
( Indentation
, toIndent
, zeroIndent
, createIndent
, indentLine
, indentMany
) where

import Mewlix.Compiler.Transpiler (Transpiler, asks, pretty)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List

newtype Indentation = Indentation { getIndent :: Int }
    deriving (Eq, Ord, Show, Enum)

-- Indentation is opaque. This is the only way to make a new Indentation value.
-- This function is intentionally a partial function.
-- The error shouldn't ever occur unless I made a big mistake somewhere.
toIndent :: Int -> Indentation
toIndent n
    | n > 0     = Indentation n
    | otherwise = error "Mewlix.Compiler.Indentation.toIndent: Indentation cannot be a negative number!"

zeroIndent :: Indentation
zeroIndent = Indentation 0

indentSize :: Int
indentSize = 4

createIndent :: Indentation -> Text
createIndent = Text.pack . flip List.replicate ' ' . (* indentSize) . getIndent

indentLine :: Indentation -> Text -> Transpiler Text
indentLine indent text = do
    prettify <- asks pretty
    if prettify
        then return (createIndent indent <> text)
        else return text

indentMany :: Indentation -> [Text] -> [Transpiler Text]
indentMany = map . indentLine
