{-# LANGUAGE LambdaCase #-}

module Meowscript.Utils.List
( listCompareM
) where

-- Monadic list comparison.
listCompareM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> [a] -> m Ordering
{-# INLINE listCompareM #-}
listCompareM _  []      []     = return EQ
listCompareM _  []      (_:_)  = return LT
listCompareM _  (_:_)   []     = return GT
listCompareM fn (x:xs)  (y:ys) = fn x y >>= \case
    EQ    -> listCompareM fn xs ys
    other -> return other
