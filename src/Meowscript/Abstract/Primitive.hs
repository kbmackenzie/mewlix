{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Abstract.Primitive
( showMeow
, prettyMeow
, meowBool
, primCompare
, primEq
, safeEq
, primSort
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Evaluate.Exception
import Meowscript.Evaluate.MeowThrower
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Meowscript.Utils.Show
import Meowscript.Utils.List
import Control.Monad.ListM (sortByM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))

{- Pretty-Printing -}
-----------------------------------------------------------------------------
showMeow :: (MonadIO m) => MeowAtom -> m Text.Text
{-# INLINE showMeow #-}
showMeow (MeowInt n) = (return . showT) n
showMeow (MeowFloat f) = (return . showT) f
showMeow (MeowString s) = (return . unboxStr) s
showMeow (MeowBool b) = (return . showT) b
showMeow (MeowList xs) = do
    items <- mapM showMeow (unboxList xs)
    (return . Text.concat) [ "[", Text.intercalate ", " items, "]" ]
showMeow (MeowBox x) = do
    let unpack (key, ref) = fmap (key,) $ liftIO (readRef ref) >>= showMeow
    let pretty (key, prim) = Text.concat [ key, ": ", prim ]
    items <- map pretty <$> mapM unpack (HashMap.toList x)
    (return . Text.concat) [ "[", Text.intercalate ", " items, "]" ]
showMeow (MeowFunc _) = return "<function>"
showMeow MeowNil = return "<nil>"

prettyMeow :: (MonadIO m) => MeowAtom -> m Text.Text
{-# INLINE prettyMeow #-}
prettyMeow (MeowString s) = (return . showT . unboxStr) s
prettyMeow other = showMeow other

{- Truthy/Falsy -}
-----------------------------------------------------------------------------
meowBool :: MeowAtom -> Bool
meowBool MeowNil = False
meowBool (MeowBool b) = b
meowBool (MeowList xs) = (not . null . unboxList) xs
meowBool (MeowString b) = (not . Text.null . unboxStr) b
meowBool (MeowBox x) = (not . HashMap.null) x
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
-- List comparison:
MeowList as  `primCompare` MeowList bs  = mappend (listLen as `compare` listLen bs) <$>
    listCompareM primCompare (unboxList as) (unboxList bs)
-- Box comparison:
MeowBox a    `primCompare` MeowBox b    = mappend (HashMap.size a `compare` HashMap.size b) <$> do
    let unpack (key, ref) = (key,) <$> liftIO (readRef ref)
    let pairComp (k1, p1) (k2, p2) = mappend (k1 `compare` k2) <$> primCompare p1 p2
    as <- mapM unpack (HashMap.toList a)
    bs <- mapM unpack (HashMap.toList b)
    listCompareM pairComp as bs
_ `primCompare` _ = throwException undefined -- TODO

primEq :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m Bool
{-# INLINE primEq #-}
primEq a b = (== EQ) <$> primCompare a b

safeEq :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m Bool
{-# INLINE safeEq #-}
safeEq a b = let f = const (return False) in catchException (primEq a b) f

{- Sorting -}
-----------------------------------------------------------------------------
primSort :: (MonadIO m, MeowThrower m) => [MeowAtom] -> m [MeowAtom]
primSort = sortByM primCompare
