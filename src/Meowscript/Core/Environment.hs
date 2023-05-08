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
, followTrail
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

modifyVar :: Key -> Prim -> Evaluator ()
modifyVar key value = lookUpVar key >>= \case
    Nothing -> throwError "not found!!!!!!!"
    (Just x) -> liftIO $ writeIORef x value

localEnv :: Evaluator Environment
localEnv = ask >>= liftIO . readIORef >>= liftIO . newIORef

runLocal :: Evaluator a -> Evaluator a
runLocal action = localEnv >>= \x -> local (const x) action

followTrail :: [Key] -> Evaluator PrimRef
followTrail [] = throwError "empty!!!!!!!"
followTrail (x:xs) = lookUp x >>= \case
    (MeowObject obj) -> innerFollow xs obj
    _ -> throwError "Not object!" 

objectLookup :: Key -> ObjectMap -> Evaluator PrimRef
objectLookup key obj = case Map.lookup key obj of
    Nothing -> throwError "not found" 
    (Just x) -> return x

innerFollow :: [Key] -> ObjectMap -> Evaluator PrimRef
innerFollow [] _ = throwError "Empty trail!" 
innerFollow [key] obj = objectLookup key obj
innerFollow (key:xs) obj = objectLookup key obj >>= evalRef >>= \case
    (MeowObject obj') -> innerFollow xs obj'
    _ -> throwError "Not object!"

createObject :: [(Key, Prim)] -> Evaluator ObjectMap
createObject [] = return Map.empty
createObject xs = do
    let asRef (key, value) = (liftIO . newIORef) value <&> (key,)
    pairs <- mapM asRef xs
    return $ Map.fromList pairs
