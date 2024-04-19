{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Parser.Nesting
( NestingFlag(..)
, Nesting
, nested
, isNested
) where

import Data.Set (Set)
import qualified Data.Set as Set

data NestingFlag =
      InLoop
    | InClass
    | InTryCatch 
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

newtype Nesting = Nesting { getNesting :: Set NestingFlag }
    deriving (Eq, Show, Monoid, Semigroup)

nested :: NestingFlag -> Nesting -> Nesting
nested flag = Nesting . Set.insert flag . getNesting

isNested :: NestingFlag -> Nesting -> Bool
isNested flag = Set.member flag . getNesting
