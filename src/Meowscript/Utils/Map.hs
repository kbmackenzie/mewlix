module Meowscript.Utils.Map
( expandAsMap
) where

import qualified Data.Map.Strict as Map

expandAsMap :: (Ord a) => [([a], b)] -> Map.Map a b
expandAsMap [] = Map.empty
expandAsMap ((keys, value):xs) = foldl (\acc k -> Map.insert k value acc) (expandAsMap xs) keys
