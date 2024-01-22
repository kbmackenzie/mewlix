{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Abstract.Module
( ModuleName(..)
, joinName
, defaultName
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

newtype ModuleName = ModuleName { getModuleName :: NonEmpty Text }
    deriving (Eq, Ord)

instance Show ModuleName where
    show = Text.unpack . joinName

joinName :: ModuleName -> Text
joinName = Text.intercalate "." . NonEmpty.toList . getModuleName

defaultName :: ModuleName -> Text
defaultName = NonEmpty.last . getModuleName
