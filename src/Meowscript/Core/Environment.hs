 {-# LANGUAGE OverloadedStrings #-} 
 {-# LANGUAGE LambdaCase #-}

module Meowscript.Core.Environment
( keyExists
, stackPush
, stackPop
, addToTop
, lookUpVar
, insertVar
, envModify
, lookUpTrail
, insertWithTrail
) where

import Meowscript.Core.AST
import Meowscript.Core.Evaluate
import Meowscript.Core.Exceptions
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.State (get, put)
import Control.Monad.Except (throwError)
import Data.List (find)
import Data.Functor ((<&>))

{- Environment -}
keyExists :: Key -> Evaluator Bool
{-# INLINABLE keyExists #-}
keyExists key = get <&> any (Map.member key)

stackPush :: Evaluator ()
{-# INLINABLE stackPush #-}
stackPush = get >>= (put . (Map.empty :))

stackPop :: Evaluator ()
{-# INLINABLE stackPop #-}
stackPop = get >>= (put . tail)

addToTop :: Key -> Prim -> Evaluator ()
{-# INLINABLE addToTop  #-}
addToTop key value = do
    (x:xs) <- get
    let x' = Map.insert key value x
    put (x':xs)

lookUpVar :: Key -> Evaluator Prim
{-# INLINABLE lookUpVar #-}
lookUpVar key = get >>= \stack -> case find (Map.member key) stack of
    (Just env) -> case Map.lookup key env of
        (Just item) -> return item
        Nothing -> throwError (badKey key)
    _ -> throwError (badKey key)

insertVar :: Text.Text -> Prim -> Evaluator ()
{-# INLINABLE insertVar #-}
insertVar key value = do
    stack <- get
    x <- keyExists key
    if x then do
        put (envModify stack key value)
    else do addToTop key value

envModify :: EnvStack -> Key -> Prim -> EnvStack
{-# INLINABLE envModify #-}
envModify [] _ _ = []
envModify (x:xs) key value =
    let hasKey = Map.member key x
        x' = Map.insert key value x
    in if hasKey
       then x':xs
       else x : envModify xs key value


{- Trails ~ Looking Up -}

lookUpTrail :: [Key] -> Evaluator Prim
lookUpTrail [] = throwError emptyTrail
lookUpTrail [key] = lookUpVar key
lookUpTrail (key:xs) = lookUpVar key >>= \case
    (MeowObject objMap) -> innerTrail xs objMap
    (MeowTrail ts) -> lookUpTrail (ts ++ xs)
    _ -> throwError (badBox key)

innerTrail :: [Key] -> ObjectMap -> Evaluator Prim
innerTrail [] _ = throwError emptyTrail
innerTrail [key] objMap = case Map.lookup key objMap of
    (Just value) -> return value
    Nothing -> throwError (badKey key)
innerTrail (key:xs) objMap = case Map.lookup key objMap of
    (Just (MeowObject newMap)) -> innerTrail xs newMap
    (Just _) -> throwError (badBox key)
    Nothing -> throwError (badKey key)


{- Trails ~ Assigning -}

insertWithTrail :: [Key] -> Prim -> Evaluator ()
insertWithTrail [] _ = throwError emptyTrail
insertWithTrail [key] value = insertVar key value
insertWithTrail (key:xs) value = lookUpVar key >>= \case
    (MeowObject objMap) -> insertVar key . MeowObject =<< modifyTrail xs value objMap 
    (MeowTrail trail) -> insertWithTrail (trail ++ xs) value
    _ -> throwError (badBox key)

modifyTrail :: [Key] -> Prim -> ObjectMap -> Evaluator ObjectMap
modifyTrail [] _ _ = throwError emptyTrail
modifyTrail [key] value objMap = return (Map.insert key value objMap)
modifyTrail (key:xs) value objMap = case Map.lookup key objMap of
    (Just (MeowObject newO)) -> do
        obj <- modifyTrail xs value newO
        return (Map.insert key (MeowObject obj) objMap)
    Nothing -> do 
        -- Insert new key if it doesn't exist.
        obj <- modifyTrail xs value Map.empty
        return (Map.insert key (MeowObject obj) objMap)
    _ -> throwError (badBox key)
