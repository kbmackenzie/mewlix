{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Mewlix.Interpreter.Classes
( asClass
, instantiate
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import qualified Data.HashMap.Internal.Strict as HashMap
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (maybe)
import Control.Monad.Except (MonadError)
import Mewlix.Parser.Keywords (meowSuper)

asClass :: (MonadError CatException m) => MeowPrim -> m MeowClass
asClass prim = case prim of
    (MeowClassDef x) -> return x
    _ -> throwError =<< undefined -- todo!

-- Class variables are not allowed.
-- Assume no class variables, only methods.

instantiate :: (MonadIO m) => MeowClass -> m CatBox
instantiate klass = do
    maybeParent <- mapM instantiate (classParent klass)
    parent      <- case maybeParent of
        Nothing  -> return HashMap.empty
        (Just p) -> HashMap.singleton meowSuper <$> (newRef . MeowBox) p

    let funcmap = classFuncs klass
    ref <- newRef HashMap.empty
    let catbox = CatBox ref

    -- Turn all functions into methods!
    methods <- mapM (newRef . MeowMFunc . MeowMethod catbox) funcmap 
    let instanceMap = methods <> parent

    modifyRef (const instanceMap) ref
    return catbox

-- The constructor needs to be called *after* this.
-- This module cannot call the constructor. Circular dependencies!
