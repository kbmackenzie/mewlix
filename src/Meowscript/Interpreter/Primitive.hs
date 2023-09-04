{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Interpreter.Primitive
( meowBool
, primCompare
, primEq
, safeEq
, primSort
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Interpreter.Exceptions
import Meowscript.Evaluate.MeowThrower
import Meowscript.Data.Stack (Stack)
import qualified Meowscript.Data.Stack as Stack
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Meowscript.Utils.List
import Control.Monad.ListM (sortByM)
import Control.Monad.IO.Class (MonadIO(..))

{- Truthy/Falsy -}
-----------------------------------------------------------------------------
meowBool :: MeowAtom -> Bool
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

primCompare :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m Ordering
{-# INLINE primCompare #-}

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
a `primCompare` b = throwException =<< operationException "comparison" [a, b]


-- A few more specific functions:
primEq :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m Bool
{-# INLINE primEq #-}
primEq a b = (== EQ) <$> primCompare a b

safeEq :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m Bool
{-# INLINE safeEq #-}
safeEq a b = let f = const (return False) in catchException (primEq a b) f


{- Sorting -}
-----------------------------------------------------------------------------
primSort :: (MonadIO m, MeowThrower m) => Stack MeowAtom -> m (Stack MeowAtom)
primSort = fmap Stack.fromList . sortByM primCompare . Stack.toList
