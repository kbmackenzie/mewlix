{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Meowscript.Evaluate.Evaluator
( Evaluator(..)
, getState
, askState
, setState
-- Re-exports
, MeowEnvironment(..)
, MeowThrower(..)
, MonadIO(..)
, SafeIO(..)
) where

import Meowscript.Evaluate.Environment
import Meowscript.Evaluate.MeowThrower
import Meowscript.Evaluate.Exception
import Meowscript.Evaluate.State
import Meowscript.IO.SafeIO
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (MonadError(..))

newtype Evaluator p a = Evaluator
    { runEvaluator :: EvaluatorState p -> IO (EvaluatorState p, Either CatException a) }

instance Functor (Evaluator e) where
    {-# INLINE fmap #-}
    fmap f m = Evaluator $ \state -> do
        (newState, ma) <- runEvaluator m state
        return (newState, fmap f ma)

instance Applicative (Evaluator e) where
    {-# INLINE pure #-}
    pure a = Evaluator $ \state -> return (state, Right a)
    {-# INLINE (<*>) #-}
    f <*> ma = Evaluator $ \state -> do
        (newState, mf) <- runEvaluator f state
        case mf of 
            (Left e)  -> return (newState, Left e)
            (Right k) -> fmap (fmap k) <$> runEvaluator ma newState

instance Monad (Evaluator e) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    m >>= f = Evaluator $ \state -> do
        (newState, ma) <- runEvaluator m state
        case ma of
            (Left e)  -> return (newState, Left e)
            (Right a) -> runEvaluator (f a) newState

instance MonadIO (Evaluator e) where
    {-# INLINE liftIO #-}
    liftIO m = Evaluator $ \state -> do
        a <- m
        return (state, Right a)

instance MeowThrower (Evaluator p) where
    {-# INLINE throwException #-}
    throwException e = Evaluator $ \state -> return (state, Left e)
    {-# INLINE catchException #-}
    catchException m f = Evaluator $ \state -> do
        (newState, ma) <- runEvaluator m state
        case ma of
            (Left e)  -> runEvaluator (f e) newState
            (Right a) -> return (newState, Right a)

instance MeowEnvironment p (Evaluator p) where
    {-# INLINE lookUpRef #-}
    lookUpRef key = Evaluator $ \state -> do
        let !ctx = evaluatorCtx state
        !ref <- contextSearch key ctx
        return (state, Right ref)
    {-# INLINE contextGet #-}
    contextGet f = Evaluator $ \state -> do
        let !ctx = evaluatorCtx state
        return (state, (Right . f) ctx)
    {-# INLINE contextSet #-}
    contextSet f = Evaluator $ \state -> do
        let !ctx = evaluatorCtx state
        let !newState = state { evaluatorCtx = f ctx }
        return (newState, Right ())
    {-# INLINE runLocal #-}
    runLocal m ctx = Evaluator $ \state -> do
        let !modifiedState = state { evaluatorCtx = ctx }
        (_, ma) <- runEvaluator m modifiedState
        case ma of
            (Left e)  -> return (state, Left e)
            (Right a) -> return (state, Right a)

instance SafeIO (Evaluator p) where
    {-# INLINE safeIO #-}
    safeIO m = Evaluator $ \state -> do
        ma <- m
        case ma of
            (Left e)  -> return (state, makeException e)
            (Right a) -> return (state, Right a)
        where makeException = Left . CatException MeowBadIO

instance MonadError CatException (Evaluator p) where
    {-# INLINE throwError #-}
    throwError = throwException
    {-# INLINE catchError #-}
    catchError = catchException

----------------------------------------------------------------------
{- Utils -}
----------------------------------------------------------------------
getState :: Evaluator p (EvaluatorState p)
{-# INLINE getState #-}
getState = Evaluator $ \state -> return (state, Right state)

askState :: (EvaluatorState p -> a) -> Evaluator p a
{-# INLINE askState #-}
askState f = Evaluator $ \state -> return (state, (Right . f) state)

setState :: (EvaluatorState p -> EvaluatorState p) -> Evaluator p ()
{-# INLINE setState #-}
setState f = Evaluator $ \state -> let !newState = f state in return (newState, Right ())
