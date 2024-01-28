{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Data.Key
( Key(..)
) where

import Data.Text (Text)
import Data.Hashable (Hashable)

newtype Key = Key { getKey :: Text }
    deriving (Eq, Ord, Show, Hashable, Semigroup, Monoid)
