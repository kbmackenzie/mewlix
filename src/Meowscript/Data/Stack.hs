{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

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
, length
, singleton
, (++|)
, concat
, concatMap
, peekMaybe
, reverse
, null
, compareM
, select
, partition
) where

import Prelude hiding (lookup, concat, concatMap, reverse, length, null, mapM)
import Data.Foldable (foldl')
import Control.Monad.IO.Class (MonadIO(..))

data Stack a =
      !a ::| !(Stack a)
    | Bottom
    deriving (Show, Functor, Foldable, Traversable)

instance (Eq a) => Eq (Stack a) where
    Bottom      == Bottom       = True
    Bottom      == _            = False
    _           == Bottom       = False
    (x ::| xs)  == (y ::| ys)   = x == y && xs == ys

instance (Ord a) => Ord (Stack a) where
    Bottom      `compare` Bottom        = EQ
    Bottom      `compare` _             = LT
    _           `compare` Bottom        = GT
    (x ::| xs)  `compare` (y ::| ys)    = case compare x y of
        EQ      -> compare xs ys
        other   -> other

instance Semigroup (Stack a) where
    (<>) = (++|)

instance Monoid (Stack a) where
    mempty = Bottom

instance Applicative Stack where
    pure = singleton
    fs <*> xs = concatMap (`fmap` xs) fs

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

peek :: Stack a -> a
peek Bottom = error "Meowscript.Data.Stack.peek: Cannot peek empty stack"
peek (x ::| _) = x

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

length :: Stack a -> Int
length Bottom = 0
length (_ ::| xs) = 1 + length xs

singleton :: a -> Stack a
singleton = (::| Bottom)

(++|) :: Stack a -> Stack a -> Stack a
Bottom      ++| other   = other
other       ++| Bottom  = other
(x ::| xs)  ++| ys      = x ::| (xs ++| ys)

concat :: Stack (Stack a) -> Stack a
concat Bottom = Bottom
concat (x ::| xs) = x ++| concat xs

concatMap :: (a -> Stack b) -> Stack a -> Stack b
concatMap f = foldr ((++|) . f) Bottom

peekMaybe :: Stack a -> Maybe a
peekMaybe Bottom = Nothing
peekMaybe (x ::| _) = Just x

reverse :: Stack a -> Stack a
reverse = foldl' (flip (::|)) Bottom

null :: Stack a -> Bool
null Bottom = True
null _      = False

compareM :: (Monad m) => (a -> a -> m Ordering) -> Stack a -> Stack a -> m Ordering
compareM _ Bottom      Bottom      = return EQ
compareM _ Bottom      _           = return LT
compareM _ _           Bottom      = return GT
compareM f (x ::| xs)  (y ::| ys)  = f x y >>= \case
    EQ      -> compareM f xs ys
    other   -> return other

select :: (a -> Bool) -> Stack a -> (Stack a, Stack a) -> (Stack a, Stack a)
select _ Bottom     (as, bs) = (as, bs)
select f (x ::| xs) (as, bs) = if f x
    then select f xs (x ::| as, bs)
    else select f xs (as, x ::| bs)

partition :: (a -> Bool) -> Stack a -> (Stack a, Stack a)
partition fn stack = select fn stack (empty, empty)
