{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Interpreter.Primitive
( meowBool
, asBox
, asString
, primCompare
, primEq
, safeEq
, primCopy
, primSort
, primHash
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Data.Key (Key)
import Mewlix.Interpreter.Exceptions
import Mewlix.Data.Stack (Stack)
import qualified Mewlix.Data.Stack as Stack
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Mewlix.Utils.List
import Control.Monad.ListM (sortByM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (MonadError(..))
import Data.Hashable (hash)
import Data.Bits (xor)
import Control.Monad ((>=>))

{- Truthy/Falsy -}
-----------------------------------------------------------------------------
meowBool :: MeowPrim -> Bool
{-# INLINABLE meowBool #-}
meowBool MeowNil        = False
meowBool (MeowBool b)   = b
meowBool (MeowStack xs)  = (not . Stack.null . unboxStack) xs
meowBool (MeowString b) = (not . Text.null . unboxStr) b
meowBool _ = True

{- Type Coercion -}
-----------------------------------------------------------------------------
asBox :: (MonadIO m, MonadError CatException m) => MeowPrim -> m CatBox
asBox prim = case prim of
    (MeowBox box) -> return box
    _             -> throwError =<< expectedBox [prim]

asString :: (MonadIO m, MonadError CatException m) => MeowPrim -> m Key
asString prim = case prim of
    (MeowString str) -> (return . unboxStr) str
    _                -> throwError =<< expectedString [prim]


{- Comparison -}
-----------------------------------------------------------------------------
-- A generic comparison function.
-- It throws exceptions with type mismatches.

primCompare :: (MonadIO m, MonadError CatException m) => MeowPrim -> MeowPrim -> m Ordering
{-# INLINABLE primCompare #-}

-- Numeric comparison:
MeowInt a    `primCompare` MeowInt b    = return (a `compare` b)
MeowInt a    `primCompare` MeowFloat b  = return (fromIntegral a `compare` b)
MeowFloat a  `primCompare` MeowInt b    = return (a `compare` fromIntegral b)
MeowFloat a  `primCompare` MeowFloat b  = return (a `compare` b)

-- Nil comparison:
MeowNil      `primCompare` MeowNil      = return EQ
MeowNil      `primCompare` _            = return LT
_            `primCompare` MeowNil      = return GT

-- String comparison:
MeowString a `primCompare` MeowString b = return (a `compare` b)

-- Boolean comparison:
MeowBool a   `primCompare` MeowBool b   = return (a `compare` b)
MeowBool a   `primCompare` b            = return (a `compare` meowBool b)
a            `primCompare` MeowBool b   = return (meowBool a `compare` b)

-- Stack comparison:
MeowStack as  `primCompare` MeowStack bs  = mappend (stackLen as `compare` stackLen bs) <$>
    Stack.compareM primCompare (unboxStack as) (unboxStack bs)

-- Box comparison:
MeowBox a    `primCompare` MeowBox b    = do
    let pairComp :: (MonadIO m, MonadError CatException m) => (Key, MeowPrim) -> (Key, MeowPrim) -> m Ordering
        pairComp (k1, p1) (k2, p2) = mappend (k1 `compare` k2) <$> primCompare p1 p2

    as <- HashMap.toList <$> unpackBox a
    bs <- HashMap.toList <$> unpackBox b
    listCompareM pairComp as bs

-- Invalid comparisons:
a `primCompare` b = throwError =<< operationException "comparison" [a, b]

-- A few more specific functions:
primEq :: (MonadIO m, MonadError CatException m) => MeowPrim -> MeowPrim -> m Bool
{-# INLINABLE primEq #-}
primEq a b = (== EQ) <$> primCompare a b

safeEq :: (MonadIO m, MonadError CatException m) => MeowPrim -> MeowPrim -> m Bool
{-# INLINABLE safeEq #-}
safeEq a b = let f = const (return False) in catchError (primEq a b) f

{- Copy -}
-----------------------------------------------------------------------------
primCopy :: (MonadIO m) => MeowPrim -> m MeowPrim
primCopy (MeowStack stack) = do
    copies <- mapM primCopy (unboxStack stack)
    (return . MeowStack . toBoxedStack) copies
primCopy (MeowBox box) = do
    boxmap <- (readRef . getBox) box
    copies <- mapM (readRef >=> primCopy) boxmap
    MeowBox <$> packBox copies
primCopy other = return other

{- Sorting -}
-----------------------------------------------------------------------------
primSort :: (MonadIO m, MonadError CatException m) => Stack MeowPrim -> m (Stack MeowPrim)
primSort = fmap Stack.fromList . sortByM primCompare . Stack.toList

{- Hashing-}
-----------------------------------------------------------------------------
primHash :: (MonadIO m) => MeowPrim -> m Int
primHash (MeowInt x)    = (return . hash) x
primHash (MeowFloat x)  = (return . hash) x
primHash (MeowString x) = (return . hash . unboxStr) x
primHash (MeowBool x)   = (return . hash) x
primHash (MeowStack x)  = foldr xor 0 <$> mapM primHash (unboxStack x)
primHash (MeowBox x)    = do
    values <- mapM readRef . HashMap.elems =<< (readRef . getBox) x
    foldr xor 0 <$> mapM primHash values
primHash _              = return 0
