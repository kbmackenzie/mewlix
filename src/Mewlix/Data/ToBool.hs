module Mewlix.Data.ToBool
( ToBool(..)
) where

{- An util for converting data types to boolean values. -}
class ToBool a where
    toBool :: a -> Bool
