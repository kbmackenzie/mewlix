{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.Keys
( keyLookup 
, assignment
, ensureValue
, assignNew
, extractKey
) where

import Meowscript.Core.AST
import Meowscript.Core.Environment
import qualified Data.Text as Text

keyLookup :: KeyType -> Evaluator Prim
{-# INLINABLE keyLookup #-}
keyLookup key = case key of
    (KeyModify x) -> lookUp x
    (KeyNew x) -> lookUp x
    (KeyTrail xs) -> lookUpTrail xs

assignment :: KeyType -> Prim -> Evaluator ()
{-# INLINABLE assignment #-}
assignment key value = case key of
    (KeyModify x) -> insertVar x value False
    (KeyNew x) -> insertVar x value True
    (KeyTrail xs) -> insertTrail xs value

ensureValue :: Prim -> Evaluator Prim
{-# INLINABLE ensureValue #-}
ensureValue (MeowKey key) = keyLookup key >>= ensureValue
ensureValue x = return x

assignNew :: Key -> Prim -> Evaluator ()
{-# INLINABLE assignNew #-}
assignNew key value = insertVar key value True

extractKey :: KeyType -> Text.Text
{-# INLINABLE extractKey #-}
extractKey (KeyModify x) = x
extractKey (KeyNew x) = x
extractKey (KeyTrail xs) = Text.intercalate "." xs
