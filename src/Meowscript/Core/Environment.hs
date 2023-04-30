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

-- Inlining pragmas.
{-# INLINE lookUpVar #-}
{-# INLINE keyExists #-}
{-# INLINE addToTop  #-}

{- Environment -}
keyExists :: Key -> Evaluator Bool
keyExists key = get <&> any (Map.member key)

stackPush :: Evaluator ()
stackPush = get >>= (put . (Map.empty :))

stackPop :: Evaluator ()
stackPop = get >>= (put . tail)

addToTop :: Key -> Prim -> Evaluator ()
addToTop key value = do
    (x:xs) <- get
    let x' = Map.insert key value x
    put (x':xs)

lookUpVar :: Key -> Evaluator Prim
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
lookUpTrail [x] = lookUpVar x

lookUpTrail (x:xs) = lookUpVar x >>= \case
    (MeowObject o) -> case innerTrail xs o of
        (Just value) -> return value
        _ -> throwError "Item doesn't exist!"
    (MeowTrail ts) -> lookUpTrail (ts ++ xs)
    _ -> throwError (Text.concat [showT x, " isn't a box!"])


innerTrail :: [Key] -> ObjectMap -> Maybe Prim
innerTrail [] _ = Nothing
innerTrail [x] o = Map.lookup x o

innerTrail (x:xs) o = case Map.lookup x o of
    (Just (MeowObject o')) -> innerTrail xs o'
    _ -> Nothing


{- Trails ~ Assigning -}

insertWithTrail :: [Key] -> Prim -> Evaluator ()

insertWithTrail [] _ = throwError "Trail cannot be empty!"
insertWithTrail [x] value = insertVar x value

insertWithTrail (x:xs) value = lookUpVar x >>= \case
    (MeowObject o) -> insertVar x . MeowObject =<< modifyTrail xs value o 
    (MeowTrail ts) -> insertWithTrail (ts ++ xs) value
    _ -> throwError (Text.concat [showT x, " isn't a box!"])


modifyTrail :: [Key] -> Prim -> ObjectMap -> Evaluator ObjectMap

modifyTrail [] _ _ = throwError "Trail cannot be empty!"
modifyTrail [x] value o = return (Map.insert x value o)

modifyTrail (x:xs) value o = case Map.lookup x o of
    -- (Just (MeowObject o')) -> modifyTrail xs value o' >>= \obj -> return (Map.insert x (MeowObject obj) o)
    (Just (MeowObject o')) -> flip (Map.insert x . MeowObject) o `fmap` modifyTrail xs value o'
    _ -> throwError "Critical error: Trail ends abruptly!"
