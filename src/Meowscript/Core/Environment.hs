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
import Control.Monad (join)

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

allocNew :: Prim -> Evaluator PrimRef
allocNew = liftIO . newIORef

-- Trail.

lookUpTrail :: [Key] -> Evaluator PrimRef
lookUpTrail [] = throwError "empty!!!!!!!"
lookUpTrail (x:xs) = lookUpVar x >>= \case
    (Just ref) -> innerLookup xs ref
    Nothing -> throwError "Key doesn't exist!" 

innerLookup :: [Key] -> PrimRef -> Evaluator PrimRef
innerLookup [] _ = throwError "Empty trail!"
innerLookup [key] ref = tapObject key ref
innerLookup (key:xs) ref = tapObject key ref >>= innerLookup xs

{- Object Handling -}
objectLookup :: Key -> ObjectMap -> Evaluator PrimRef
objectLookup key obj = case Map.lookup key obj of
    (Just x) -> return x
    Nothing -> throwError "Key not found" 

createObject :: [(Key, Prim)] -> Evaluator ObjectMap
createObject [] = return Map.empty
createObject xs = do
    let asRef (key, value) = (liftIO . newIORef) value <&> (key,)
    pairs <- mapM asRef xs
    return $ Map.fromList pairs

modifyObject :: PrimRef -> (ObjectMap -> ObjectMap) -> Evaluator ()
modifyObject ref f = evalRef ref >>= \case
    (MeowObject obj) -> (liftIO . writeIORef ref . MeowObject . f) obj
    _ -> throwError "??????????"

tapObject :: Key -> Prim -> Evaluator PrimRef
tapObject (MeowObject x) = objectLookup key 
tapObject _ = throwError "Not object!"

{- Trail: Insert -}

insertTrail :: [Key] -> Prim -> Evaluator ()
insertTrail [] _ = throwError "aaaaaaaaaaaaaaaaaaaaaaaa"
insertTrail [key] value = insertVar key value False
insertTrail (key:xs) value = lookUpVar key >>= \case
    (Just ref) -> innerInsert xs value ref
    Nothing -> throwError "nooooooooooooooooo"

innerInsert :: [Key] -> Prim -> PrimRef -> Evaluator ()
innerInsert [] _ _ = throwError "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
innerInsert [key] value ref = allocNew value >>= modifyObject ref . Map.insert key
innerInsert (key:xs) value ref = evalRef ref >>= tapObject key >>= \case
    (Just ref') -> innerInsert xs value ref'


    Nothing -> do --todo: consider taking this off, maybe...? :'o
        ref' <- createObject [] >>= allocNew . MeowObject
        modifyObject ref (Map.insert key ref')
        innerInsert xs value ref'


{- Trail : Actions -}

type Callback = PrimRef -> Evaluator Prim

trailAction :: [Key] -> Callback -> Evaluator Prim
trailAction
{-
trailAction :: [Key] -> Callback -> Evaluator Prim
trailAction [] _ = throwError "aaaaaaaaaaaaaaaaaaaaaa"
trailAction [key] f = lookUpVar key >>= \case
    (Just x) -> f x
    Nothing -> throwError "asdffdasdfs"
trailAction (key:key':xs) f = lookUpVar key >>= \case
    (Just ref) -> evalRef ref >>= \case
        (MeowModule env) -> local (const env) (trailAction xs f)
        (MeowObject obj) -> objectLookup key' obj >>= innerLookup' xs f
        _ -> throwError "not object!!!!!!!!"
    Nothing -> throwError "asdfdfdsff"

innerLookup' :: [Key] -> Callback -> PrimRef -> Evaluator Prim
innerLookup' [] f ref = f ref
innerLookup' (x:xs) f ref = evalRef ref >>= \case
    (MeowModule env) -> local (const env) (trailAction xs f)
    (MeowObject obj) -> objectLookup x obj >>= innerLookup' xs f
    _ -> throwError "not object!!!!!!!!"
-}
