{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Interpreter.Boxes
( getBox
, boxPeek
, boxWrite
, asIdentifier
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Interpreter.Exceptions
import Mewlix.Parser.AST
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class(MonadIO(..))

asBox :: (MonadIO m, MonadError CatException m) => MeowPrim -> m CatBox
asBox atom = case atom of
    (MeowBox box) -> return box
    _             -> throwError =<< notABoxException [atom]

boxPeek :: (MonadIO m, MonadError CatException m) => Identifier -> MeowPrim -> m (Ref MeowPrim)
boxPeek key atom = do
    !box <- asBox atom >>= readRef . getBox
    case HashMap.lookup key box of
        Nothing       -> throwError =<< notAPropertyException key [atom]
        (Just !ref)    -> return ref

boxWrite :: (MonadIO m, MonadError CatException m) => Identifier -> MeowPrim -> Ref MeowPrim -> m ()
boxWrite key value atomRef = do
    !catbox <- readRef atomRef >>= asBox
    let !boxRef = getBox catbox
    !box <- readRef boxRef
    case HashMap.lookup key box of
        Nothing       -> do
            !ref <- newRef value
            modifyRef (HashMap.insert key ref) boxRef
        (Just ref)    -> writeRef value ref

asIdentifier :: (MonadIO m, MonadError CatException m) => MeowPrim -> m Identifier
asIdentifier value = case value of
    (MeowString str) -> (return . unboxStr) str
    _                -> throwError =<< notAnIdentifier [value]
