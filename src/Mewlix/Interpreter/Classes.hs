{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Interpreter.Classes
( asClass
, instantiate
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import qualified Data.HashMap.Internal.Strict as HashMap
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError)
import Mewlix.Parser.Keywords (meowSuper)

{- Helpers: -}
--------------------------------------------------------------
asClass :: (MonadError CatException m) => MeowPrim -> m MeowClass
asClass prim = case prim of
    (MeowClassDef x) -> return x
    _ -> throwError =<< undefined -- todo!


{- Instantiate: -}
--------------------------------------------------------------
{- The constructor is *not* called by this function.
 - It's expected to be called *after* this function! -}

instantiate :: (MonadIO m) => MeowClass -> m CatBox
instantiate classDef = do
    maybeParent <- mapM instantiate (classParent classDef)
    super <- case maybeParent of
        Nothing -> return HashMap.empty
        (Just parent) -> do
            value <- newRef (MeowBox parent)
            return (HashMap.singleton meowSuper value)

    ref <- newRef HashMap.empty
    let classInstance = CatBox ref
    let funcmap = classFuncs classDef

    -- Turn all functions into methods!
    methods <- mapM (newRef . MeowMFunc . MeowMethod classInstance) funcmap 
    let instanceMap = methods <> super

    modifyRef (const instanceMap) ref
    return classInstance
