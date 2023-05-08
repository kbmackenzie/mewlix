module Meowscript.Core.Keys
( keyLookup 
, assignment
, ensureValue
, funcLookup
) where

import Meowscript.Core.AST
import Meowscript.Core.Environment

keyLookup :: KeyType -> Evaluator Prim
keyLookup key = case key of
    (KeyModify x) -> lookUp x
    (KeyNew x) -> lookUp x
    (KeyTrail xs) -> lookUpTrail xs

assignment :: KeyType -> Prim -> Evaluator ()
assignment key value = case key of
    (KeyModify x) -> insertVar x value False
    (KeyNew x) -> insertVar x value True
    (KeyTrail xs) -> insertTrail xs value

ensureValue :: Prim -> Evaluator Prim
ensureValue (MeowKey key) = keyLookup key >>= ensureValue
ensureValue x = return x

funcLookup :: KeyType -> FnCallback -> Evaluator Prim
funcLookup key f = case key of 
    (KeyModify x) -> runFunction x f
    (KeyNew x) -> runFunction x f
    (KeyTrail xs) -> runMethod xs f
