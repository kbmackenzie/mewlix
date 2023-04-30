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
import Meowscript.Core.Messages
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.State (get, put)
import Control.Monad.Except (throwError)
import Data.List (find)
import Data.Functor ((<&>))

{- Environment -}
keyExists :: Key -> Evaluator Bool
{-# INLINE keyExists #-}
keyExists key = get <&> any (Map.member key)

stackPush :: Evaluator ()
stackPush = get >>= (put . (Map.empty :))

stackPop :: Evaluator ()
stackPop = get >>= (put . tail)

addToTop :: Key -> Prim -> Evaluator ()
{-# INLINE addToTop  #-}
addToTop key value = do
    (x:xs) <- get
    let x' = Map.insert key value x
    put (x':xs)

lookUpVar :: Key -> Evaluator Prim
{-# INLINE lookUpVar #-}
lookUpVar key = get >>= \stack -> case find (Map.member key) stack of
    (Just env) -> case Map.lookup key env of
        (Just item) -> return item
        Nothing -> throwError (Text.concat [ "Key doesn't exist: ", key])
    _ -> throwError (Text.concat [ "Key doesn't exist: ", key ])

insertVar :: Text.Text -> Prim -> Evaluator ()
insertVar key value = do
    stack <- get
    x <- keyExists key
    if x then do
        put (envModify stack key value)
    else do addToTop key value

envModify :: EnvStack -> Key -> Prim -> EnvStack
envModify [] _ _ = []
envModify (x:xs) key value =
    let hasKey = Map.member key x
        x' = Map.insert key value x
    in if hasKey
       then x':xs
       else x : envModify xs key value


{- Trails ~ Looking Up -}

lookUpTrail :: [Key] -> Evaluator Prim
lookUpTrail [] = throwError "Trail is empty!"
lookUpTrail [key] = lookUpVar key
lookUpTrail (key:xs) = lookUpVar key >>= \case
    (MeowObject objMap) -> case innerTrail xs objMap of
        (Just value) -> return value
        _ -> throwError "Item doesn't exist!"
    (MeowTrail ts) -> lookUpTrail (ts ++ xs)
    _ -> throwError (Text.concat [showT key, " isn't a box!"])

--

innerTrail :: [Key] -> ObjectMap -> Maybe Prim
innerTrail [] _ = Nothing
innerTrail [key] objMap = Map.lookup key objMap
innerTrail (key:xs) objMap = case Map.lookup key objMap of
    (Just (MeowObject newO)) -> innerTrail xs newO
    _ -> Nothing


{- Trails ~ Assigning -}

insertWithTrail :: [Key] -> Prim -> Evaluator ()

insertWithTrail [] _ = throwError "Trail cannot be empty!"
insertWithTrail [key] value = insertVar key value

insertWithTrail (key:xs) value = lookUpVar key >>= \case
    (MeowObject objMap) -> insertVar key . MeowObject =<< modifyTrail xs value objMap 
    (MeowTrail ts) -> insertWithTrail (ts ++ xs) value
    _ -> throwError (Text.concat [showT key, " isn't a box!"])

--

modifyTrail :: [Key] -> Prim -> ObjectMap -> Evaluator ObjectMap

modifyTrail [] _ _ = throwError "Trail cannot be empty!"
modifyTrail [key] value objMap = return (Map.insert key value objMap)

modifyTrail (key:xs) value objMap = case Map.lookup key objMap of
    (Just (MeowObject newO)) -> do
        obj <- modifyTrail xs value newO
        return (Map.insert key (MeowObject obj) objMap)
    Nothing -> do --If key doesn't exist, add it.
        obj <- modifyTrail xs value Map.empty
        return (Map.insert key (MeowObject obj) objMap)
    _ -> throwError (Text.concat ["Key ", key, " is not an object!"])
