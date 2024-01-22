{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Abstract.Module
( ModuleName(..)
, joinName
) where

import Data.Text (Text)
import qualified Data.Text as Text

newtype ModuleName = ModuleName { getModuleName :: [Text] }
    deriving (Eq, Ord)

instance Show ModuleName where
    show = Text.unpack . joinName

joinName :: ModuleName -> Text
joinName = Text.intercalate "." . getModuleName
