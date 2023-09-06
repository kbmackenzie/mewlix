{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Meowscript.Interpreter.Primitive
( meowBool
, primCompare
, primEq
, safeEq
, primCopy
, primSort
, primHash
) where

import Meowscript.Abstract.Meow
import Meowscript.Data.Ref
import Meowscript.Data.Key (Key)
import Meowscript.Interpreter.Exceptions
import Meowscript.Data.Stack (Stack)
import qualified Meowscript.Data.Stack as Stack
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Meowscript.Utils.List
import Control.Monad.ListM (sortByM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (MonadError(..))
import Data.Hashable (hash)
import Data.Bits (xor)

{- Truthy/Falsy -}
-----------------------------------------------------------------------------
meowBool :: MeowPrim -> Bool
{-# INLINABLE meowBool #-}
meowBool MeowNil        = False
meowBool (MeowBool b)   = b
meowBool (MeowStack xs)  = (not . Stack.null . unboxStack) xs
meowBool (MeowString b) = (not . Text.null . unboxStr) b
--meowBool (MeowBox x)    = (not . HashMap.null) x
meowBool _ = True

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
    let pairs = fmap HashMap.toList . readRef . getBox
    let unpack (key, ref) = (key,) <$> liftIO (readRef ref)
    let pairComp (k1, p1) (k2, p2) = mappend (k1 `compare` k2) <$> primCompare p1 p2
    as <- mapM unpack =<< pairs a
    bs <- mapM unpack =<< pairs b
    listCompareM pairComp as bs
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
    let copyPair (key, ref) = do
            copied <- readRef ref >>= primCopy >>= newRef
            return (key, copied)
    pairs <- HashMap.toList <$> (readRef . getBox) box
    copies <- mapM copyPair pairs
    let copyBox = HashMap.fromList copies
    toCatBox copyBox
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
