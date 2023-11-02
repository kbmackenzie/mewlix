{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Interpreter.Boxes
( asBox
, getBox
, boxPeek
, boxWrite
, asIdentifier
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Interpreter.Exceptions
import Mewlix.Parser.AST
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class(MonadIO(..))

asBox :: (MonadIO m, MonadError CatException m) => MeowPrim -> m CatBox
asBox prim = case prim of
    (MeowBox box) -> return box
    _             -> throwError =<< notABoxException [prim]

boxPeek :: (MonadIO m, MonadError CatException m) => Identifier -> CatBox -> m (Ref MeowPrim)
boxPeek key box = do
    !valueRef <- catBoxGet key box
    case valueRef of
        Nothing        -> throwError =<< notAPropertyException key [MeowBox box]
        (Just !ref)    -> return ref

boxWrite :: (MonadIO m) => Identifier -> MeowPrim -> CatBox-> m ()
boxWrite key value catbox = do
    !maybeRef <- catBoxGet key catbox
    case maybeRef of
        Nothing    -> catBoxPut key value catbox
        (Just ref) -> writeRef value ref

asIdentifier :: (MonadIO m, MonadError CatException m) => MeowPrim -> m Identifier
asIdentifier value = case value of
    (MeowString str) -> (return . unboxStr) str
    _                -> throwError =<< notAnIdentifier [value]
