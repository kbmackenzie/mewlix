{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.Environment
( newEnv
, lookUp
, lookUpVar
, keyExists
, createVar
, modifyVar
, insertVar
, overwriteVar
, localEnv
, runLocal
, evalRef
) where

import Meowscript.Core.AST
import Data.IORef
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.Reader (asks, ask, liftIO, local)
import Control.Monad.Except (throwError)
import Data.Functor ((<&>))

newEnv :: IO Environment
newEnv = newIORef Map.empty

lookUpVar :: Key -> Evaluator (Maybe PrimRef)
lookUpVar key = (ask >>= liftIO . readIORef) <&> Map.lookup key

evalRef :: PrimRef -> Evaluator Prim
evalRef = liftIO . readIORef

lookUp :: Key -> Evaluator Prim
lookUp key = lookUpVar key >>= \case
    Nothing -> throwError "todo"
    (Just x) -> (liftIO . readIORef) x

keyExists :: Key -> Evaluator Bool
keyExists key = (ask >>= liftIO . readIORef) <&> Map.member key

createVar :: Key -> Prim -> Evaluator ()
createVar key value = do
    env <- ask >>= liftIO . readIORef
    value' <- (liftIO . newIORef) value
    let env' = Map.insert key value' env
    ask >>= liftIO . flip writeIORef env'

modifyVar :: PrimRef -> Prim -> Evaluator ()
modifyVar key value = liftIO $ writeIORef key value

insertVar :: Key -> Prim -> LocalNew -> Evaluator ()
insertVar key value True = overwriteVar key value
insertVar key value False = lookUpVar key >>= \case
    Nothing -> createVar key value
    (Just x) -> modifyVar x value

overwriteVar :: Key -> Prim -> Evaluator ()
{-# INLINE overwriteVar #-}
overwriteVar = createVar

localEnv :: Evaluator Environment
localEnv = ask >>= liftIO . readIORef >>= liftIO . newIORef

runLocal :: Evaluator a -> Evaluator a
runLocal action = localEnv >>= \x -> local (const x) action

objectLookup :: Key -> ObjectMap -> Evaluator PrimRef
objectLookup key obj = case Map.lookup key obj of
    Nothing -> throwError "Key not found" 
    (Just x) -> return x

createObject :: [(Key, Prim)] -> Evaluator ObjectMap
createObject [] = return Map.empty
createObject xs = do
    let asRef (key, value) = (liftIO . newIORef) value <&> (key,)
    pairs <- mapM asRef xs
    return $ Map.fromList pairs

lookUpTrail :: [Key] -> Evaluator PrimRef
lookUpTrail [] = throwError "empty!!!!!!!"
lookUpTrail (x:xs) = lookUpVar x >>= \case
    (Just prim) -> innerFollow xs prim
    _ -> throwError "Key doesn't exist!" 

asObject :: Key -> PrimRef -> Evaluator PrimRef
asObject key ref = evalRef ref >>= \case
    (MeowObject obj) -> objectLookup key obj
    _ -> throwError "Not object!"

innerFollow :: [Key] -> PrimRef -> Evaluator PrimRef
innerFollow [] _ = throwError "Empty trail!" 
innerFollow [key] ref = asObject key ref
innerFollow (key:xs) obj = asObject key obj >>= innerFollow xs


-- This is the main insert function.
insertTo :: [Key] -> Prim -> LocalNew -> Evaluator PrimRef
insertTo [] _ _ = throwError "?????? empty"
insertTo [key] value l = insertVar key value l
insertTo (x:xs) key value _ = 
