{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Meowscript.Interpreter.Import
( meowImport
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Evaluate.Evaluator
import Meowscript.Evaluate.State
import Meowscript.Evaluate.Environment
import Meowscript.Data.Key
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap

filterKeys :: HashMap.HashMap Key a -> HashMap.HashMap Key a
{-# INLINABLE filterKeys #-}
filterKeys = HashMap.filterWithKey publicKey
    where publicKey k _ = (not . Text.isPrefixOf "_") k

contextImport :: a -> (a -> Evaluator s b) -> EvaluatorState s -> Evaluator s ()
{-# INLINABLE contextImport #-}
contextImport a m boxedState = Evaluator $ \state -> do
    (importState, ma) <- runEvaluator (m a) boxedState
    case ma of
        (Left e)  -> return (state, Left e)
        _         -> do
            let !globalRef = (globalEnv . evaluatorCtx) state
            !env        <- readRef globalRef
            !importEnv  <- (readRef . globalEnv . evaluatorCtx) importState
            let !newEnv = Environment $ getEnv env <> (filterKeys . getEnv) importEnv
            writeRef newEnv globalRef
            return (state, Right ())

meowImport :: Maybe Key -> a -> (a -> Evaluator MeowAtom b) -> EvaluatorState MeowAtom -> Evaluator MeowAtom ()
{-# INLINABLE meowImport #-}
meowImport qualified a m boxedState = case qualified of
    Nothing     -> contextImport a m boxedState
    (Just key)  -> Evaluator $ \state -> do
        (importState, ma) <- runEvaluator (m a) boxedState
        case ma of
            (Left e) -> return (state, Left e)
            _        -> do
                let !env = (currentEnv . evaluatorCtx) state
                !importEnv <- (readRef . globalEnv . evaluatorCtx) importState
                !newBox    <- newRef . MeowBox . CatBox =<< (newRef . filterKeys . getEnv) importEnv
                let f = Environment . HashMap.insert key newBox . getEnv
                modifyRef f env
                return (state, Right ())
