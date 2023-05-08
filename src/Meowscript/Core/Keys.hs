module Meowscript.Core.Keys
( ensureValue
, assignment
) where

import Meowscript.Core.AST
import Meowscript.Core.Environment

ensureValue :: Prim -> Evaluator Prim
ensureValue (MeowKey key) = ensureValue =<< case key of
    (KeyModify x) -> lookUp x
    (KeyNew x) -> lookUp x
    (KeyTrail xs) -> lookUpTrail xs
ensureValue x = return x

assignment :: KeyType -> Prim -> Evaluator ()
assignment key value = case key of
    (KeyModify x) -> insertVar x value False
    (KeyNew x) -> insertVar x value True
    (KeyTrail xs) -> insertTrail xs value
