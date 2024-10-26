{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Abstract.Module
( ModuleData(..)
, ModuleKey(..)
, joinKey
, hasAlias
, defaultName
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Mewlix.Abstract.Key (Key(..))
import Data.Maybe (isJust)
import Data.Hashable (Hashable)

data ModuleData = ModuleData
    { moduleKey   :: ModuleKey
    , moduleAlias :: Maybe Key  }
    deriving (Eq, Show)

newtype ModuleKey = ModuleKey { getModuleKey :: NonEmpty Text }
    deriving (Eq, Ord, Hashable)

instance Show ModuleKey where
    show = Text.unpack . joinKey

joinKey :: ModuleKey -> Text
joinKey = Text.intercalate "." . NonEmpty.toList . getModuleKey

hasAlias :: ModuleData -> Bool
hasAlias = isJust . moduleAlias

defaultName :: ModuleData -> Text
defaultName = NonEmpty.last . getModuleKey . moduleKey
