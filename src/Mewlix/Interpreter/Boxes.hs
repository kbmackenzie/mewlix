{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Interpreter.Boxes
( asBox
, getBox
, boxPeek
, boxWrite
, boxFlatten
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Data.Key (Key)
import Mewlix.Interpreter.Primitive
import Mewlix.Interpreter.Exceptions
import Mewlix.Parser.Keywords (meowClass, meowSuper)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class(MonadIO)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad ((>=>))

boxPeek :: (MonadIO m, MonadError CatException m) => Key -> CatBox -> m (Ref MeowPrim)
boxPeek key box = do
    !valueRef <- catBoxGet key box
    case valueRef of
        Nothing        -> do
            parent <- catBoxGet meowSuper box
            case parent of
                Nothing     -> throwError =<< notAPropertyException key [MeowBox box]
                (Just !ref) -> (readRef >=> asBox >=> boxPeek key) ref
        (Just !ref)    -> return ref

boxWrite :: (MonadIO m) => Key -> MeowPrim -> CatBox-> m ()
boxWrite key value box = do
    !maybeRef <- catBoxGet key box
    case maybeRef of
        Nothing    -> catBoxPut key value box
        (Just ref) -> writeRef value ref

boxFlatten :: (MonadIO m, MonadError CatException m) => CatBox -> m (HashMap Key MeowPrim)
boxFlatten box = do
    let reserved :: [Key]
        reserved = [ meowClass, meowSuper ]

    let filterKeys :: HashMap Key MeowPrim -> HashMap Key MeowPrim
        filterKeys = HashMap.filterWithKey (\key _ -> key `elem` reserved)

    !rawMap <- unpackBox box
    !parent <- mapM asBox (HashMap.lookup meowSuper rawMap)

    !parentMap <- case parent of
        (Just xs)   -> boxFlatten xs
        Nothing     -> return HashMap.empty

    (return . filterKeys) $ rawMap <> parentMap
