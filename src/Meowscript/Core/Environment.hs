--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.Environment
( newEnv
, lookUp
, lookUpVar
, lookUpRef
, keyExists
, createVar
, modifyVar
, insertVar
, insertRef
, insertMany
, overwriteVar
, localEnv
, runLocal
, runClosure
, newMeowRef
, readMeowRef
, writeMeowRef
, modifyMeowRef
, createObject
, insertObject
, modifyObject
, primAsBox
, peekObject 
, peekAsObject
, emptyLib
) where

import Meowscript.Core.AST
import Data.IORef
import Meowscript.Core.Exceptions
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as Map
import Data.Functor ((<&>))
import Data.Foldable (foldl')
import Lens.Micro.Platform (set)

{- Helpers -}
-------------------------------------------------------------
newEnv :: IO Environment
{-# INLINE newEnv #-}
newEnv = newIORef Map.empty

{- IORef Utils -}
newMeowRef :: (MonadIO m) => a -> m (IORef a)
{-# INLINE newMeowRef #-}
newMeowRef = liftIO . newIORef

readMeowRef :: (MonadIO m) => IORef a -> m a
{-# INLINE readMeowRef #-}
readMeowRef = liftIO . readIORef

writeMeowRef :: (MonadIO m) => IORef a -> a -> m ()
{-# INLINE writeMeowRef #-}
writeMeowRef = (liftIO .) . writeIORef

modifyMeowRef :: (MonadIO m) => IORef a -> (a -> a) -> m ()
{-# INLINE modifyMeowRef #-}
modifyMeowRef = (liftIO .) . modifyIORef

emptyLib :: IO ObjectMap
{-# INLINE emptyLib #-}
emptyLib = return Map.empty

{- Variables -}
-------------------------------------------------------------
lookUpVar :: Key -> Evaluator (Maybe PrimRef)
{-# INLINE lookUpVar #-}
lookUpVar key = (asks meowEnv >>= readMeowRef) <&> Map.lookup key

lookUpRef :: Key -> Evaluator PrimRef
{-# INLINE lookUpRef #-}
lookUpRef key = lookUpVar key >>= \case
    (Just x) -> return x
    Nothing -> throwError (badKey key)

keyExists :: Key -> Evaluator Bool
{-# INLINE keyExists #-}
keyExists key = (asks meowEnv >>= readMeowRef) <&> Map.member key

lookUp :: Key -> Evaluator Prim
{-# INLINE lookUp #-}
lookUp key = lookUpRef key >>= readMeowRef

createVar :: Key -> Prim -> Evaluator ()
{-# INLINE createVar #-}
createVar key value = do
    env <- asks meowEnv >>= readMeowRef
    value' <- newMeowRef value
    let env' = Map.insert key value' env
    asks meowEnv >>= flip writeMeowRef env'

modifyVar :: PrimRef -> Prim -> Evaluator ()
{-# INLINE modifyVar #-}
modifyVar = writeMeowRef

insertVar :: Key -> Prim -> Overwrite -> Evaluator ()
{-# INLINE insertVar #-}
insertVar key value True = overwriteVar key value
insertVar key value False = lookUpVar key >>= \case
    Nothing -> createVar key value
    (Just x) -> modifyVar x value

insertRef :: Key -> PrimRef -> Evaluator ()
{-# INLINE insertRef #-}
insertRef key ref = do
    env <- asks meowEnv >>= readMeowRef
    let env' = Map.insert key ref env
    asks meowEnv >>= flip writeMeowRef env'

overwriteVar :: Key -> Prim -> Evaluator ()
{-# INLINE overwriteVar #-}
overwriteVar = createVar

insertMany :: [(Key, PrimRef)] -> Evaluator ()
insertMany pairs = do
    env <- asks meowEnv >>= readMeowRef
    let insertPair m (key, ref) = Map.insert key ref m
    let env' = foldl' insertPair env pairs
    asks meowEnv >>= flip writeMeowRef env'

localEnv :: Evaluator Environment
{-# INLINE localEnv #-}
localEnv = asks meowEnv >>= readMeowRef >>= newMeowRef

runLocal :: Evaluator a -> Evaluator a
{-# INLINE runLocal #-}
runLocal action = set meowEnvL <$> localEnv <*> ask
    >>= \ctx -> local (const ctx) action

runClosure :: ObjectMap -> Evaluator a -> Evaluator a
{-# INLINE runClosure #-}
runClosure closure action = set meowEnvL <$> newMeowRef closure <*> ask
    >>= \ctx -> local (const ctx) action


{- Object Handling -}
-------------------------------------------------------------
peekObject :: Key -> ObjectMap -> Evaluator PrimRef
{-# INLINE peekObject #-}
peekObject key obj = case Map.lookup key obj of
    (Just x) -> return x
    Nothing -> throwError =<< badDot key (MeowObject obj)

createObject :: [(Key, Prim)] -> IO ObjectMap
{-# INLINE createObject #-}
createObject [] = return Map.empty
createObject xs = do
    let asRef (key, value) = newIORef value <&> (key,)
    pairs <- mapM asRef xs
    return $ Map.fromList pairs

modifyObject :: PrimRef -> (ObjectMap -> ObjectMap) -> Evaluator ()
{-# INLINE modifyObject #-}
modifyObject ref f = readMeowRef ref >>= \case
    (MeowObject obj) -> (writeMeowRef ref . MeowObject . f) obj
    x -> throwError =<< notBox x

insertObject :: PrimRef -> Key -> Prim -> Evaluator ()
{-# INLINE insertObject #-}
insertObject ref key value = readMeowRef ref >>= \case
    (MeowObject obj) -> if Map.member key obj
        then writeMeowRef (obj Map.! key) value
        else do
            valueRef <- newMeowRef value
            writeMeowRef ref $ MeowObject (Map.insert key valueRef obj)
    x -> throwError =<< notBox x

primAsBox :: Prim -> Evaluator ObjectMap
{-# INLINE primAsBox #-}
primAsBox (MeowObject x) = return x
primAsBox x = throwError =<< notBox x

peekAsObject :: Key -> Prim -> Evaluator PrimRef
{-# INLINE peekAsObject #-}
peekAsObject key (MeowObject x) = peekObject key x
peekAsObject _ x = throwError =<< notBox x
