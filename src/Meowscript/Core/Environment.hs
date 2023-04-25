 {-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Environment
( keyExists
, stackPush
, stackPop
, addToTop
, lookUpVar
, insertVar
, envModify
) where

import Meowscript.Core.AST
import Meowscript.Core.Evaluate
import Meowscript.Core.Messages
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.State (get, put)
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
        (Just env) -> return (env Map.! key)
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
