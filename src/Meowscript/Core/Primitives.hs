{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.Primitives
( primCompare
, primEq
, primSort
, primCopy
, primHash
) where

import Meowscript.Core.AST
import Meowscript.Core.Exceptions
import Meowscript.Core.Environment
import qualified Data.Map.Strict as Map
import Control.Monad.Except (throwError)
import Control.Monad.ListM (sortByM)
import Control.Monad ((>=>))
import Data.Hashable (hash)
import Data.Bits (xor)

{- Comparison (==, <, >, <=, >=) operations. -}
---------------------------------------------------------
primCompare :: Prim -> Prim -> Evaluator Ordering
{-# INLINE primCompare #-}
-- Numbers
MeowInt a    `primCompare` MeowInt b    = return (a `compare` b)
MeowInt a    `primCompare` MeowDouble b = return (fromIntegral a `compare` b)
MeowDouble a `primCompare` MeowInt b    = return (a `compare` fromIntegral b)
MeowDouble a `primCompare` MeowDouble b = return (a `compare` b)
-- Nil
MeowLonely   `primCompare` MeowLonely   = return EQ
MeowLonely   `primCompare` _            = return LT
_            `primCompare` MeowLonely   = return GT
-- Strings
MeowString a `primCompare` MeowString b = return (a `compare` b)
-- Booleans
MeowBool a   `primCompare` MeowBool b   = return (a `compare` b)
MeowBool a   `primCompare` b            = return (a `compare` meowBool b)
a            `primCompare` MeowBool b   = return (meowBool a `compare` b)
-- Lists
MeowList a   `primCompare` MeowList b   = listCompareM primCompare a b
-- Objects
MeowObject a `primCompare` MeowObject b = do
    let unpackBox = mapM (\(key, ref) -> (key,) <$> readMeowRef ref) . Map.toList
    let comparePair (k1, p1) (k2, p2) = ((k1 `compare` k2) `mappend`) <$> primCompare p1 p2
    as <- unpackBox a
    bs <- unpackBox b
    listCompareM comparePair as bs
a `primCompare` b = throwError =<< opException "Comparison" [a, b]

-- Monadic list comparison.
listCompareM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> [a] -> m Ordering
listCompareM _  []      []     = return EQ
listCompareM _  []      (_:_)  = return LT
listCompareM _  (_:_)   []     = return GT
listCompareM fn (x:xs)  (y:ys) = fn x y >>= \case
    EQ    -> listCompareM fn xs ys
    other -> return other


{- Equality (a == b) operation. -}
---------------------------------------------------------
primEq :: Prim -> Prim -> Evaluator Bool
{-# INLINE primEq #-}
a `primEq` b = (== EQ) <$> (a `primCompare` b)


{- Sorting operation. -}
---------------------------------------------------------
primSort :: [Prim] -> Evaluator [Prim]
{-# INLINE primSort #-}
primSort = sortByM primCompare


{- Deep-copying. -}
---------------------------------------------------------
primCopy :: Prim -> Evaluator Prim
{-# INLINE primCopy #-}
primCopy (MeowList xs) = MeowList <$> mapM primCopy xs
primCopy (MeowObject x) = do
    newValues <- mapM (readMeowRef >=> primCopy >=> newMeowRef) (Map.elems x)
    let keys = Map.keys x
    (return . MeowObject . Map.fromList) (zip keys newValues)
primCopy x = return x


{- Hashing -}
---------------------------------------------------------
primHash :: Prim -> Evaluator Int
{-# INLINE primHash #-}
primHash (MeowInt x) = return (hash x)
primHash (MeowDouble x) = return (hash x)
primHash (MeowString x) = return (hash x)
primHash (MeowBool x) = return (hash x)
primHash (MeowList xs) = foldr xor 0 <$> mapM primHash xs
primHash (MeowObject obj) = do
    let xs = Map.toList obj
    let unpack (key, ref) = (key,) <$> readMeowRef ref
    let hashPair (x, y) = (hash x `xor`) <$> primHash y
    foldr xor 0 <$> mapM (unpack >=> hashPair) xs
primHash MeowLonely = return 0
primHash x = throwError =<< badHash x
