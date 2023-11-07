{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Interpreter.Classes
( asClass
, instantiate
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Data.Key (Key)
import Mewlix.Abstract.Meowable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Internal.Strict as HashMap
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError)
import Control.Monad ((>=>))
import Mewlix.Parser.Keywords (meowSuper, meowClass)
import Mewlix.Interpreter.Exceptions (notAClassException)

{- Notes: Class instances have two special properties:
 -
 -      meowSuper -> the superclass property :: MeowBox
 -      meowClass -> the class name property :: MeowString
 -
 - In all valid class instances in Mewlix, at least the
 - latter will be present.
 - If their types diverge from the example, that's a huge error. -}


{- Helpers: -}
--------------------------------------------------------------
asClass :: (MonadError CatException m, MonadIO m) => MeowPrim -> m MeowClass
asClass prim = case prim of
    (MeowClassDef c) -> return c
    other            -> throwError =<< notAClassException [other]


{- Instantiate: -}
--------------------------------------------------------------
{- The constructor is *not* called by this function.
 - It's expected to be called *after* this function! -}

type BoxTransform = HashMap Key PrimRef -> HashMap Key PrimRef

instantiate :: (MonadIO m) => MeowClass -> m CatBox
instantiate classDef = do
    parent <- mapM (instantiate >=> newRef . MeowBox) (classParent classDef)
    let addParent :: BoxTransform
        addParent = case parent of
            Nothing     -> id
            (Just ref)  -> HashMap.insert meowSuper ref

    name  <- (toMeow . className >=> newRef) classDef
    let addName :: BoxTransform
        addName = HashMap.insert meowClass name

    let transforms :: [BoxTransform]
        transforms = [ addParent, addName ]

    boxReference <- newRef HashMap.empty
    let funcMap = classFuncs classDef
    let classInstance = CatBox boxReference

    -- Turn all functions into methods!
    methods <- mapM (newRef . MeowMFunc . MeowMethod classInstance) funcMap 
    let instanceMap = foldr ($) methods transforms

    modifyRef (const instanceMap) boxReference
    return classInstance
