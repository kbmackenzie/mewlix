{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Meowscript.Interpreter.Import
( addImport
) where

import Meowscript.Abstract.Meow
import Meowscript.Data.Ref
import Meowscript.Abstract.State
import Meowscript.Data.Key
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap

filterKeys :: HashMap Key a -> HashMap Key a
{-# INLINABLE filterKeys #-}
filterKeys = HashMap.filterWithKey publicKey
    where publicKey k _ = (not . Text.isPrefixOf "_") k

addImport :: Maybe Key -> Environment MeowPrim -> Evaluator ()
addImport qualified impEnv = do
    let !imp = (Environment . filterKeys . getEnv) impEnv
    env <- asks evaluatorEnv
    case qualified of
        Nothing     -> do
            modifyRef (<> imp) env
        (Just name) -> do
            boxRef <- newRef (getEnv imp)
            let !box = MeowBox CatBox { getBox = boxRef }
            contextDefine name box
