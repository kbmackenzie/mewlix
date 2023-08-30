{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Meowscript.Evaluate.Evaluator
( EvaluatorState(..)
, ModuleInfo(..)
, EvaluatorMeta(..)
, Evaluator(..)
, getState
, askState
, setState
-- Re-exports
, MeowEnvironment(..)
, MeowThrower(..)
, MonadIO(..)
) where

import Meowscript.Evaluate.Environment
import Meowscript.Evaluate.MeowThrower
import Meowscript.Evaluate.Exception
import Control.Monad.IO.Class (MonadIO(..))

data EvaluatorState p = EvaluatorState
    { evaluatorCtx  :: Context p
    , moduleInfo    :: ModuleInfo
    , evaluatorMeta :: EvaluatorMeta p }

data ModuleInfo = ModuleInfo
data EvaluatorMeta p = EvaluatorMeta

newtype Evaluator p a = Evaluator
    { runEvaluator :: EvaluatorState p -> IO (EvaluatorState p, Either CatException a) }

instance Functor (Evaluator e) where
    fmap f m = Evaluator $ \state -> do
        (newState, ma) <- runEvaluator m state
        return (newState, fmap f ma)

instance Applicative (Evaluator e) where
    pure a = Evaluator $ \state -> return (state, Right a)
    f <*> ma = Evaluator $ \state -> do
        (newState, mf) <- runEvaluator f state
        case mf of 
            (Left e) -> return (newState, Left e)
            (Right k) -> fmap (fmap k) <$> runEvaluator ma newState

instance Monad (Evaluator e) where
    return = pure
    m >>= f = Evaluator $ \state -> do
        (newState, ma) <- runEvaluator m state
        case ma of
            (Left e) -> return (newState, Left e)
            (Right a) -> runEvaluator (f a) newState

instance MonadIO (Evaluator e) where
    liftIO m = Evaluator $ \state -> do
        a <- m
        return (state, Right a)

instance MeowThrower (Evaluator p) where
    throwException e = Evaluator $ \state -> return (state, Left e)
    catchException m f = Evaluator $ \state -> do
        (newState, ma) <- runEvaluator m state
        case ma of
            (Left e) -> runEvaluator (f e) newState
            (Right a) -> return (newState, Right a)

instance MeowEnvironment p (Evaluator p) where
    lookUpRef key = Evaluator $ \state -> do
        let ctx = evaluatorCtx state
        !ref <- contextSearch key ctx
        return (state, Right ref)
    contextGet f = Evaluator $ \state -> do
        let ctx = evaluatorCtx state
        return (state, (Right . f) ctx)
    contextSet f = Evaluator $ \state -> do
        let ctx = evaluatorCtx state
        let newState = state { evaluatorCtx = f ctx }
        return (newState, Right ())
    runLocal m ctx = Evaluator $ \state -> do
        let modifiedState = state { evaluatorCtx = ctx }
        (_, ma) <- runEvaluator m modifiedState
        case ma of
            (Left e) -> return (state, Left e)
            (Right a) -> return (state, Right a)

----------------------------------------------------------------------
{- Utils -}
----------------------------------------------------------------------
getState :: Evaluator p (EvaluatorState p)
getState = Evaluator $ \state -> return (state, Right state)

askState :: (EvaluatorState p -> a) -> Evaluator p a
askState f = Evaluator $ \state -> return (state, (Right . f) state)

setState :: (EvaluatorState p -> EvaluatorState p) -> Evaluator p ()
setState f = Evaluator $ \state -> let !newState = f state in return (newState, Right ())
