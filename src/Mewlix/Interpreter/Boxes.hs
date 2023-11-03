{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Interpreter.Boxes
( asBox
, getBox
, boxPeek
, boxWrite
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Data.Key (Key)
import Mewlix.Interpreter.Exceptions
import Mewlix.Parser.Keywords (meowSuper)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad ((>=>))

asBox :: (MonadIO m, MonadError CatException m) => MeowPrim -> m CatBox
asBox prim = case prim of
    (MeowBox box) -> return box
    _             -> throwError =<< notABoxException [prim]

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
boxWrite key value catbox = do
    !maybeRef <- catBoxGet key catbox
    case maybeRef of
        Nothing    -> catBoxPut key value catbox
        (Just ref) -> writeRef value ref
