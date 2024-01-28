{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Abstract.Module
( Module(..)
, ModuleKey(..)
, joinKey
, hasAlias
, defaultName
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Mewlix.Data.Key (Key(..))
import Data.Maybe (isJust)

data Module = Module
    { moduleKey   :: ModuleKey
    , moduleAlias :: Maybe Key  }
    deriving (Show)

newtype ModuleKey = ModuleKey { getModuleKey :: NonEmpty Text }
    deriving (Eq, Ord)

instance Show ModuleKey where
    show = Text.unpack . joinKey

joinKey :: ModuleKey -> Text
joinKey = Text.intercalate "." . NonEmpty.toList . getModuleKey

hasAlias :: Module -> Bool
hasAlias = isJust . moduleAlias

defaultName :: Module -> Text
defaultName = NonEmpty.last . getModuleKey . moduleKey
