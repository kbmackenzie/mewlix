module Meowscript.Utils.Map
( expandAsMap
, sortTupleList
) where

import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Function (on)

expandAsMap :: (Ord a) => [([a], b)] -> Map.Map a b
{-# INLINABLE expandAsMap #-}
expandAsMap [] = Map.empty
expandAsMap ((keys, value):xs) = foldl (\acc k -> Map.insert k value acc) (expandAsMap xs) keys

sortTupleList :: (Ord a) => [(a, b)] -> [(a, b)]
{-# INLINE sortTupleList #-}
sortTupleList = sortBy (compare `on` fst)
