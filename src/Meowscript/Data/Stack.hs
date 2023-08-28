{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Meowscript.Data.Stack
( Stack(..)
, empty
, push
, pop
, peek
, lookup
, toList
, fromList
, uncons
, singleton
, (++|)
, concat
, concatMap
) where

import Prelude hiding (lookup, concat, concatMap)

data Stack a =
      !a ::| !(Stack a)
    | Bottom
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Semigroup (Stack a) where
    (<>) = (++|)

instance Monoid (Stack a) where
    mempty = Bottom

instance Applicative Stack where
    pure = singleton
    fs <*> xs = concatMap (<$> xs) fs

instance Monad Stack where
    return = pure
    (>>=) = flip concatMap

empty :: Stack a
empty = Bottom

push :: a -> Stack a -> Stack a
push = (::|)

pop :: Stack a -> Stack a
pop Bottom = error "Meowscript.Data.Stack.pop: Cannot pop empty stack"
pop (_ ::| xs) = xs

peek :: Stack a -> Maybe a
peek Bottom = Nothing
peek (x ::| _) = Just x

lookup :: (a -> Bool) -> Stack a -> Maybe a
lookup _ Bottom = Nothing
lookup f (x ::| xs) = if f x then Just x else lookup f xs

toList :: Stack a -> [a]
toList = foldr (:) []

fromList :: [a] -> Stack a
fromList = foldr (::|) Bottom

uncons :: Stack a -> Maybe (a, Stack a)
uncons Bottom = Nothing
uncons (x ::| xs) = Just (x, xs)

singleton :: a -> Stack a
singleton = (::| Bottom)

(++|) :: Stack a -> Stack a -> Stack a
Bottom      ++| other   = other
other       ++| Bottom  = other
(x ::| xs)  ++| ys      = xs ++| (x ::| ys)

concat :: Stack (Stack a) -> Stack a
concat Bottom = Bottom
concat (x ::| xs) = x ++| concat xs

concatMap :: (a -> Stack b) -> Stack a -> Stack b
concatMap f = foldr ((++|) . f) Bottom
