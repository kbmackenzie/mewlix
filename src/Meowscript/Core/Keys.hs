{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.Keys
( keyLookup 
, assignment
, pairAsRef
, ensureValue
, assignNew
, extractKey
, ensureKey
) where

import Meowscript.Core.AST
import Meowscript.Core.Environment
import qualified Data.Text as Text
import Meowscript.Core.Exceptions
import Meowscript.Utils.Show
import Control.Monad.Except (throwError)

keyLookup :: KeyType -> Evaluator Prim
{-# INLINABLE keyLookup #-}
keyLookup key = case key of
    (KeyModify x) -> lookUp x
    (KeyNew x) -> lookUp x
    (KeyRef x) -> pairAsRef x >>= readMeowRef

assignment :: KeyType -> Prim -> Evaluator ()
{-# INLINABLE assignment #-}
assignment key value = case key of
    (KeyModify x) -> insertVar x value False
    (KeyNew x) -> insertVar x value True
    (KeyRef x) -> pairAsRef x >>= flip writeMeowRef value

pairAsRef :: (PrimRef, Prim) -> Evaluator PrimRef
{-# INLINABLE pairAsRef #-}
pairAsRef (ref, prim) = (,) <$> ensureKey prim <*> readMeowRef ref >>= uncurry peekAsObject

ensureValue :: Prim -> Evaluator Prim
{-# INLINABLE ensureValue #-}
ensureValue (MeowKey key) = keyLookup key >>= ensureValue
ensureValue x = return x

assignNew :: Key -> Prim -> Evaluator ()
{-# INLINABLE assignNew #-}
assignNew key value = insertVar key value True

extractKey :: KeyType -> Evaluator Text.Text
{-# INLINABLE extractKey #-}
extractKey (KeyModify x) = return x
extractKey (KeyNew x) = return x
extractKey x = throwError $ meowUnexpected "Cannot extract reference as key!" (showT x)

ensureKey :: Prim -> Evaluator Text.Text
{-# INLINABLE ensureKey #-}
ensureKey (MeowKey key) = extractKey key
ensureKey x = throwError =<< badTrail [x]
