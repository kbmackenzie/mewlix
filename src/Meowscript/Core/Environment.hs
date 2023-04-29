 {-# LANGUAGE OverloadedStrings #-} 

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
import Control.Monad.State (get, put, liftIO)
import Control.Monad.Except (throwError)
import Data.List (find)

{- Environment -}
keyExists :: Key -> Evaluator Bool
keyExists key = do
    stack <- get
    let res = any (Map.member key) stack
    return res

stackPush :: Evaluator ()
stackPush = do
    stack <- get
    put $ Map.empty:stack

stackPop :: Evaluator ()
stackPop = do
    stack <- get
    put $ tail stack

addToTop :: Key -> Prim -> Evaluator ()
addToTop key value = do
    (x:xs) <- get
    let x' = Map.insert key value x
    put (x':xs)


lookUpVar :: Key -> Evaluator Prim
lookUpVar key = do
    stack <- get
    let x = find (Map.member key) stack
    case x of 
        (Just env) -> case Map.lookup key env of
            -- (Just (MeowTrail xs)) -> lookUpTrail xs
            (Just x') -> return x'
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


{- Object Trails -}

{- Looking Up -}
lookUpTrail :: [Key] -> Evaluator Prim
lookUpTrail [x] = lookUpVar x
lookUpTrail (x:xs) = do
    obj <- lookUpVar x
    case obj of
        (MeowObject o) -> case innerTrail xs o of
            (Just value) -> return value
            _ -> throwError "Item doesn't exist!"
        (MeowTrail ts) -> do
            let newTrail = ts ++ xs
            lookUpTrail newTrail
        _ -> throwError (Text.concat [showT x, " isn't a box!"])
lookUpTrail _ = throwError "Trail is empty!"

innerTrail :: [Key] -> ObjectMap -> Maybe Prim
innerTrail [] _ = Nothing
innerTrail [x] o = Map.lookup x o
innerTrail (x:xs) o = case Map.lookup x o of
    (Just (MeowObject o')) -> innerTrail xs o'
    _ -> Nothing


{- Assigning -}
insertWithTrail :: [Key] -> Prim -> Evaluator ()
insertWithTrail [] _ = throwError "Trail cannot be empty!"
insertWithTrail [x] value = insertVar x value
insertWithTrail (x:xs) value = do
    obj <- lookUpVar x
    case obj of
        (MeowObject o) -> insertVar x . MeowObject =<< modifyTrail xs value o 
        --modifyTrail xs value o >>= \n -> insertVar x (MeowObject n)
        (MeowTrail ts) -> do
            let newTrail = ts ++ xs
            insertWithTrail newTrail value
        _ -> throwError (Text.concat [showT x, " isn't a box!"])

modifyTrail :: [Key] -> Prim -> ObjectMap -> Evaluator ObjectMap
modifyTrail [] _ _ = throwError "Trail cannot be empty!"
modifyTrail [x] value o = return (Map.insert x value o)
modifyTrail (x:xs) value o = case Map.lookup x o of
    (Just (MeowObject o')) -> do
        obj <- modifyTrail xs value o'
        return (Map.insert x (MeowObject obj) o)
    _ -> throwError "Critical error: Trail ends abruptly!"
