{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Parser.Nesting
( NestingFlag(..)
, Nesting
, addNesting
, nested
, noNesting
, defineNesting
) where

import Data.Set (Set)
import qualified Data.Set as Set

data NestingFlag =
      InLoop
    | InClass
    | InFunction
    | InTryCatch 
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

newtype Nesting = Nesting { getNesting :: Set NestingFlag }
    deriving (Eq, Show, Monoid, Semigroup)

addNesting :: NestingFlag -> Nesting -> Nesting
addNesting flag = Nesting . Set.insert flag . getNesting

nested :: NestingFlag -> Nesting -> Bool
nested flag = Set.member flag . getNesting

noNesting :: Nesting -> Nesting
noNesting = const (Nesting mempty)

defineNesting :: [NestingFlag] -> Nesting -> Nesting
defineNesting = const . Nesting . Set.fromList
