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
, lookUpTrail
, insertTrail
, trailAction
, createObject
, modifyObject
, runMethod
) where

import Meowscript.Core.AST
import Data.IORef
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.Reader (ask, liftIO, local)
import Control.Monad.Except (throwError)
import Data.Functor ((<&>))

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

insertVar :: Key -> Prim -> LocalNew -> Evaluator ()
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

tapObject :: Key -> Prim -> Evaluator PrimRef
tapObject key (MeowObject x) = peekObject key x
tapObject _ _ = throwError "Not object!"


{- Trails -}

lookUpTrail :: [Key] -> Evaluator PrimRef
lookUpTrail [] = throwError "empty!!!!!!!"
lookUpTrail (x:xs) = lookUpVar' x >>= innerLookup xs

innerLookup :: [Key] -> PrimRef -> Evaluator PrimRef
innerLookup [] _ = throwError "Empty trail!"
innerLookup [key] ref = evalRef ref >>= tapObject key
innerLookup (key:xs) ref = evalRef ref >>= tapObject key >>= innerLookup xs

insertTrail :: [Key] -> Prim -> Evaluator ()
insertTrail [] _ = throwError "aaaaaaaaaaaaaaaaaaaaaaaa"
insertTrail [key] value = insertVar key value False
insertTrail (key:xs) value = lookUpVar' key >>= innerInsert xs value

innerInsert :: [Key] -> Prim -> PrimRef -> Evaluator ()
innerInsert [] _ _ = throwError "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
innerInsert [key] value ref = allocNew value >>= modifyObject ref . Map.insert key
innerInsert (key:xs) value ref = evalRef ref >>= tapObject key >>= innerInsert xs value


{- Trail : Actions -}

type Callback = PrimRef -> Evaluator Prim

trailAction :: [Key] -> Callback -> Evaluator Prim
trailAction [] _ = throwError "aaaaaaaaaaaa"
trailAction [key] f = lookUpVar' key >>= f
trailAction (k:k':xs) f = lookUp k >>= \case
    (MeowModule env) -> local (const env) (trailAction (k':xs) f)
    (MeowObject obj) -> peekObject k' obj >>= innerAction xs f
    _ -> throwError "not object!!!!!!!!!"

innerAction :: [Key] -> Callback -> PrimRef -> Evaluator Prim
innerAction [] f ref = f ref
innerAction (x:xs) f ref = evalRef ref >>= tapObject x >>= innerAction xs f

runMethod :: [Key] -> Callback -> Evaluator Prim
runMethod [] _ = throwError "??"
runMethod [_] _ = throwError "??"
runMethod keys fn = do
    let top = init keys
    let key = last keys
    trailAction top $ \x -> runLocal $ do
        insertRef "home" x
        -- do all the function stuff here
        -- return the function return
        evalRef x >>= tapObject key >>= fn
        -- the callback should take the function
        -- and push arguments to the current env
        -- and do all of that stuff > .<
        -- all that should be doen by the callback!!
