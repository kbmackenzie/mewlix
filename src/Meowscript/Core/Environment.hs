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
, overwriteVar
, localEnv
, runLocal
, runClosure
, newMeowRef
, readMeowRef
, writeMeowRef
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
import Control.Monad.Reader (asks, liftIO, local)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as Map
import Data.Functor ((<&>))

{- Helpers -}
-------------------------------------------------------------
newEnv :: IO Environment
{-# INLINABLE newEnv #-}
newEnv = newIORef Map.empty

{- IORef Utils -}
newMeowRef :: a -> Evaluator (IORef a)
{-# INLINABLE newMeowRef #-}
newMeowRef = liftIO . newIORef

readMeowRef :: IORef a -> Evaluator a
{-# INLINABLE readMeowRef #-}
readMeowRef = liftIO . readIORef

writeMeowRef :: IORef a -> a -> Evaluator ()
{-# INLINABLE writeMeowRef #-}
writeMeowRef = (liftIO .) . writeIORef

emptyLib :: IO ObjectMap
{-# INLINABLE emptyLib #-}
emptyLib = return Map.empty

{- Variables -}
-------------------------------------------------------------
lookUpVar :: Key -> Evaluator (Maybe PrimRef)
{-# INLINABLE lookUpVar #-}
lookUpVar key = (asks snd >>= readMeowRef) <&> Map.lookup key

lookUpRef :: Key -> Evaluator PrimRef
{-# INLINABLE lookUpRef #-}
lookUpRef key = lookUpVar key >>= \case
    (Just x) -> return x
    Nothing -> throwError (badKey key)

keyExists :: Key -> Evaluator Bool
{-# INLINABLE keyExists #-}
keyExists key = (asks snd >>= readMeowRef) <&> Map.member key

lookUp :: Key -> Evaluator Prim
{-# INLINABLE lookUp #-}
lookUp key = lookUpRef key >>= readMeowRef

createVar :: Key -> Prim -> Evaluator ()
{-# INLINABLE createVar #-}
createVar key value = do
    env <- asks snd >>= readMeowRef
    value' <- newMeowRef value
    let env' = Map.insert key value' env
    asks snd >>= flip writeMeowRef env'

modifyVar :: PrimRef -> Prim -> Evaluator ()
{-# INLINABLE modifyVar #-}
modifyVar = writeMeowRef

insertVar :: Key -> Prim -> Overwrite -> Evaluator ()
{-# INLINABLE insertVar #-}
insertVar key value True = overwriteVar key value
insertVar key value False = lookUpVar key >>= \case
    Nothing -> createVar key value
    (Just x) -> modifyVar x value

insertRef :: Key -> PrimRef -> Evaluator ()
{-# INLINABLE insertRef #-}
insertRef key ref = do
    env <- asks snd >>= readMeowRef
    let env' = Map.insert key ref env
    asks snd >>= flip writeMeowRef env'

overwriteVar :: Key -> Prim -> Evaluator ()
{-# INLINE overwriteVar #-}
overwriteVar = createVar

localEnv :: Evaluator Environment
{-# INLINABLE localEnv #-}
localEnv = asks snd >>= readMeowRef >>= newMeowRef

runLocal :: Evaluator a -> Evaluator a
{-# INLINABLE runLocal #-}
runLocal action = asks ((,) . fst) <*> localEnv >>= \x -> local (const x) action

runClosure :: ObjectMap -> Evaluator a -> Evaluator a
{-# INLINABLE runClosure #-}
runClosure closure action = asks ((,) . fst) <*> newMeowRef closure >>= \x -> local (const x) action


{- Object Handling -}
-------------------------------------------------------------
peekObject :: Key -> ObjectMap -> Evaluator PrimRef
{-# INLINABLE peekObject #-}
peekObject key obj = case Map.lookup key obj of
    (Just x) -> return x
    Nothing -> throwError =<< badDot key (MeowObject obj)

createObject :: [(Key, Prim)] -> IO ObjectMap
{-# INLINABLE createObject #-}
createObject [] = return Map.empty
createObject xs = do
    let asRef (key, value) = newIORef value <&> (key,)
    pairs <- mapM asRef xs
    return $ Map.fromList pairs

modifyObject :: PrimRef -> (ObjectMap -> ObjectMap) -> Evaluator ()
{-# INLINABLE modifyObject #-}
modifyObject ref f = readMeowRef ref >>= \case
    (MeowObject obj) -> (writeMeowRef ref . MeowObject . f) obj
    x -> throwError =<< notBox x

insertObject :: PrimRef -> Key -> Prim -> Evaluator ()
{-# INLINABLE insertObject #-}
insertObject ref key value = readMeowRef ref >>= \case
    (MeowObject obj) -> if Map.member key obj
        then writeMeowRef (obj Map.! key) value
        else do
            valueRef <- newMeowRef value
            writeMeowRef ref $ MeowObject (Map.insert key valueRef obj)
    x -> throwError =<< notBox x

primAsBox :: Prim -> Evaluator ObjectMap
{-# INLINABLE primAsBox #-}
primAsBox (MeowObject x) = return x
primAsBox x = throwError =<< notBox x

peekAsObject :: Key -> Prim -> Evaluator PrimRef
{-# INLINABLE peekAsObject #-}
peekAsObject key (MeowObject x) = peekObject key x
peekAsObject _ x = throwError =<< notBox x
