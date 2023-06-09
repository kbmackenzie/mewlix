{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.Keys
( keyLookup 
, keyAsRef
, assignment
, pairAsRef
, pairAsInsert
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
import qualified Data.Map.Strict as Map

keyLookup :: KeyType -> Evaluator Prim
{-# INLINABLE keyLookup #-}
keyLookup key = case key of
    (KeyModify x) -> lookUp x
    (KeyNew x) -> lookUp x
    (KeyRef x) -> pairAsRef x >>= readMeowRef

keyAsRef :: KeyType -> Evaluator PrimRef
keyAsRef key = case key of
    (KeyModify x) -> lookUpRef x
    (KeyNew x) -> lookUpRef x
    (KeyRef x) -> pairAsRef x

assignment :: KeyType -> Prim -> Evaluator ()
{-# INLINABLE assignment #-}
assignment key value = case key of
    (KeyModify x) -> insertVar x value False
    (KeyNew x) -> insertVar x value True
    (KeyRef x) -> pairAsInsert x value

pairAsRef :: (PrimRef, Prim) -> Evaluator PrimRef
{-# INLINABLE pairAsRef #-}
pairAsRef (ref, prim) = (,) <$> ensureKey prim <*> readMeowRef ref >>= uncurry peekAsObject

pairAsInsert :: (PrimRef, Prim) -> Prim -> Evaluator ()
pairAsInsert (ref, prim) value = ensureKey prim >>= \key -> do
    valref <- newMeowRef value
    modifyObject ref (Map.insert key valref)

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
ensureKey x = throwError "????????" --throwError =<< badTrail [x]
