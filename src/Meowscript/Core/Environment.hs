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
, insertRef
, overwriteVar
, localEnv
, runLocal
, allocNew
, evalRef
, createObject
, modifyObject
, Callback
, lookUpTrail
, insertTrail
, trailAction
, FnCallback
, runMethod
, runFunction
) where

import Meowscript.Core.AST
import Data.IORef
import qualified Data.Map as Map
import Control.Monad.Reader (ask, liftIO, local)
import Control.Monad.Except (throwError)
import Data.Functor ((<&>))
import Control.Monad (void)

newEnv :: IO Environment
newEnv = newIORef Map.empty

lookUpVar :: Key -> Evaluator (Maybe PrimRef)
lookUpVar key = (ask >>= liftIO . readIORef) <&> Map.lookup key

lookUpVar' :: Key -> Evaluator PrimRef
lookUpVar' key = lookUpVar key >>= \case
    (Just x) -> return x
    Nothing -> throwError "todo"

keyExists :: Key -> Evaluator Bool
keyExists key = (ask >>= liftIO . readIORef) <&> Map.member key

lookUp :: Key -> Evaluator Prim
lookUp key = lookUpVar' key >>= (liftIO . readIORef)

createVar :: Key -> Prim -> Evaluator ()
createVar key value = do
    env <- ask >>= liftIO . readIORef
    value' <- (liftIO . newIORef) value
    let env' = Map.insert key value' env
    ask >>= liftIO . flip writeIORef env'

modifyVar :: PrimRef -> Prim -> Evaluator ()
modifyVar key value = liftIO $ writeIORef key value

insertVar :: Key -> Prim -> Overwrite -> Evaluator ()
insertVar key value True = overwriteVar key value
insertVar key value False = lookUpVar key >>= \case
    Nothing -> createVar key value
    (Just x) -> modifyVar x value

insertRef :: Key -> PrimRef -> Evaluator ()
insertRef key ref = do
    env <- ask >>= liftIO . readIORef
    let env' = Map.insert key ref env
    ask >>= liftIO . flip writeIORef env'

overwriteVar :: Key -> Prim -> Evaluator ()
{-# INLINE overwriteVar #-}
overwriteVar = createVar

localEnv :: Evaluator Environment
localEnv = ask >>= liftIO . readIORef >>= liftIO . newIORef

runLocal :: Evaluator a -> Evaluator a
runLocal action = localEnv >>= \x -> local (const x) action

allocNew :: Prim -> Evaluator PrimRef
allocNew = liftIO . newIORef

evalRef :: PrimRef -> Evaluator Prim
evalRef = liftIO . readIORef


{- Object Handling -}
peekObject :: Key -> ObjectMap -> Evaluator PrimRef
peekObject key obj = case Map.lookup key obj of
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

peekAsObject :: Key -> Prim -> Evaluator PrimRef
peekAsObject key (MeowObject x) = peekObject key x
peekAsObject _ _ = throwError "Not object!"

{- Trail : Actions -}

type Callback = PrimRef -> Evaluator Prim

trailAction :: [Key] -> Callback -> Evaluator Prim
trailAction [] _ = throwError "empty trail"
trailAction (key:keys) f = lookUpVar' key >>= innerAction keys f

innerAction :: [Key] -> Callback -> PrimRef -> Evaluator Prim
innerAction [] f ref = f ref
innerAction (key:keys) f ref = evalRef ref >>= \case
    (MeowModule env) -> do
        ref' <- evalRef ref >>= peekAsObject key
        local (const env) (innerAction keys f ref')
    (MeowObject obj) -> peekObject key obj >>= innerAction keys f
    _ -> throwError "not object"

lookUpTrail :: [Key] -> Evaluator Prim
lookUpTrail keys = trailAction keys evalRef

insertTrail :: [Key] -> Prim -> Evaluator ()
insertTrail keys value
    | length keys <= 1 = throwError "aaaaa"
    | otherwise = void $ trailAction (init keys) $ \x -> do
        allocNew value >>= modifyObject x . Map.insert (last keys)
        return value


{- Meowscript Functions -}

type FnCallback = Prim -> Evaluator Prim

runMethod :: [Key] -> FnCallback -> Evaluator Prim
runMethod keys callback
    | length keys <= 1 = throwError "aaaaaa"
    | otherwise = trailAction (init keys) $ \parent -> runLocal $ do
        insertRef "home" parent
        -- Look up function:
        fn <- evalRef parent >>= peekAsObject (last keys) >>= evalRef
        -- Function callback should:
        -- 1. Push arguments to stack
        -- 2. Run the entire function
        callback fn
        
runFunction :: Key -> FnCallback -> Evaluator Prim
runFunction key f = lookUp key >>= f
