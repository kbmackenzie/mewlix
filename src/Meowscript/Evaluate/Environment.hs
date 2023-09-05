{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Evaluate.Environment
( Environment(..)
, Context(..)
, MeowEnvironment(..)
-- Context:
, contextSearch
, contextWrite
, localContext
, contextDefine
, contextMany
, initContext
, freezeLocal
-- Environment:
, createEnvironment
) where

import Meowscript.Data.Key
import Meowscript.Data.Ref
import Meowscript.Evaluate.Exception
import Meowscript.Evaluate.MeowThrower
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (MonadIO(..))

newtype Environment a = Environment
    { getEnv :: HashMap.HashMap Key (Ref a) }
    deriving (Semigroup)

data Context a = Context
    { globalEnv    :: Ref (Environment a) 
    , currentEnv   :: Ref (Environment a)
    , enclosingCtx :: Maybe (Context a)  }

class (Monad m, MonadIO m) => MeowEnvironment s m | m -> s where
    lookUpRef  :: Key -> m (Maybe (Ref s))
    contextGet :: (Context s -> a) -> m a
    contextSet :: (Context s -> Context s) -> m ()
    runLocal   :: m a -> Context s -> m a

    context :: m (Context s)
    {-# INLINE context #-}
    context = contextGet id

    lookUp :: (MeowThrower m) => Key -> m s
    {-# INLINE lookUp #-}
    lookUp key = do
        x <- lookUpRef key
        case x of
            (Just !ref) -> readRef ref
            Nothing     -> throwException CatException {
                exceptionType = MeowUnboundKey,
                exceptionMessage = Text.concat [ "Unbound key: \"", key, "\"!" ]
            }

{- Context -}
-----------------------------------------------------------------------------
contextSearch :: (MonadIO m) => Key -> Context a -> m (Maybe (Ref a))
contextSearch !key !ctx = (readRef . currentEnv) ctx >>= \env ->
    case (HashMap.lookup key . getEnv) env of
    (Just !ref) -> return (Just ref)
    Nothing     -> case enclosingCtx ctx of
        (Just enclosing) -> contextSearch key enclosing
        Nothing          -> return Nothing
        {-
        Nothing          -> do
            -- Global lookup, duh!
            global <- readRef (globalEnv ctx)
            return (HashMap.lookup key (getEnv global))
        -}

contextWrite :: (MonadIO m) => Key -> a -> Context a -> m ()
contextWrite !key !value !ctx = do
    let !env = currentEnv ctx
    !valueRef <- newRef value
    let f = Environment . HashMap.insert key valueRef . getEnv
    modifyRef f env

localContext :: (MonadIO m) => Context a -> m (Context a)
localContext !ctx = do
    newEnv <- newRef $ Environment HashMap.empty
    let !newCtx = Context {
        globalEnv = globalEnv ctx,
        currentEnv = newEnv,
        enclosingCtx = Just ctx
    }
    return newCtx

contextDefine :: (MonadIO m) => Key -> Ref a -> Context a -> m ()
contextDefine !key !ref !ctx = do
    let !env = currentEnv ctx
    let f = Environment . HashMap.insert key ref . getEnv
    modifyRef f env

contextMany :: (MonadIO m) => [(Key, Ref a)] -> Context a -> m ()
contextMany pairs ctx = do
    let !env = currentEnv ctx
    let bind (key, ref) = HashMap.insert key ref
    let makeMap = Environment . (\hashmap -> foldr bind hashmap pairs) . getEnv
    modifyRef makeMap env

initContext :: (MonadIO m) => m (Context a)
initContext = do
    !initial <- newRef $ Environment HashMap.empty
    let !ctx = Context {
        globalEnv = initial,
        currentEnv = initial,
        enclosingCtx = Nothing
    }
    return ctx

-- A helper to copy and freeze the local context
-- for use in lambda closures.
freezeLocal :: (MonadIO m) => Context a -> m (Context a)
freezeLocal ctx = do
    !newLocal <- copyRef $ currentEnv ctx
    return ctx { currentEnv = newLocal }


{- Environments -}
-----------------------------------------------------------------------------
createEnvironment :: (MonadIO m) => [(Key, a)] -> m (Environment a)
createEnvironment pairs = do
    let pack (key, ref) = (key,) <$> newRef ref
    Environment . HashMap.fromList <$> mapM pack pairs
