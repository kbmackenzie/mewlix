{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Keywords.Types
( SimpleKeyword(..)
, LongSymbol(..)
, WordSequence(..)
, SymbolOptions(..)
, joinWords
, firstWord
) where

import Data.Text (Text)
import Data.String (IsString)
import Data.Hashable (Hashable)
import GHC.IsList (IsList(..))
import qualified Data.Text as Text
import qualified Data.List as List

-- SimpleKeywords are case-sensitive.
newtype SimpleKeyword = SimpleKeyword { unwrapKeyword :: Text }
    deriving (Eq, Show, IsString, Hashable, Semigroup, Monoid)

-- Long symbols are case-insensitive.
newtype LongSymbol = LongSymbol { unwrapSymbol :: Text }
    deriving (Eq, Show, IsString, Hashable, Semigroup, Monoid)

-- A sequence of keywords. The space between them doesn't matter.
newtype WordSequence = WordSequence { unwrapWords :: [SimpleKeyword] }
    deriving (Eq, Show, Semigroup, Monoid)

-- A number of options for symbols.
newtype SymbolOptions = SymbolOptions { unwrapSymbols :: [LongSymbol] }
    deriving (Eq, Show, Semigroup, Monoid)

instance IsList WordSequence where
    type Item WordSequence = SimpleKeyword
    fromList = WordSequence
    toList = unwrapWords

instance IsList SymbolOptions where
    type Item SymbolOptions = LongSymbol
    fromList = SymbolOptions
    toList = unwrapSymbols

joinWords :: WordSequence -> Text
joinWords = Text.intercalate " " . map unwrapKeyword . unwrapWords

firstWord :: WordSequence -> SimpleKeyword
firstWord = maybe mempty fst . List.uncons . unwrapWords
