{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Keywords.Types
( Keyword(..)
, LongSymbol(..)
, WordSequence(..)
, joinWords
, firstWord
) where

import Data.Text (Text)
import Data.String (IsString)
import Data.Hashable (Hashable)
import GHC.IsList (IsList(..))
import qualified Data.Text as Text
import qualified Data.List as List

-- Keywords are case-sensitive.
newtype Keyword = Keyword { unwrapKeyword :: Text }
    deriving (Eq, Show, IsString, Hashable, Semigroup, Monoid)

-- Long symbols are case-insensitive.
newtype LongSymbol = LongSymbol { unwrapSymbol :: Text }
    deriving (Eq, Show, IsString, Hashable, Semigroup, Monoid)

-- A sequence of keywords. The space between them doesn't matter.
newtype WordSequence = WordSequence { unwrapWords :: [Keyword] }
    deriving (Eq, Show, Semigroup, Monoid)

instance IsList WordSequence where
    type Item WordSequence = Keyword
    fromList = WordSequence
    toList = unwrapWords

joinWords :: WordSequence -> Text
joinWords = Text.intercalate " " . map unwrapKeyword . unwrapWords

firstWord :: WordSequence -> Keyword
firstWord = maybe mempty fst . List.uncons . unwrapWords
