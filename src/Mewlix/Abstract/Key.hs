{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Abstract.Key
( Key(..)
) where

import Data.Text (Text)
import Data.Hashable (Hashable)

newtype Key = Key { getKey :: Text }
    deriving (Eq, Ord, Show, Hashable, Semigroup, Monoid)
