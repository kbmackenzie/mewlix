{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Core.RunEvaluator
( runMeow
, runExpr
, runMeow'
, runExpr'
) where

import Meowscript.Core.AST
import Meowscript.Core.Base
import Meowscript.Core.Blocks
import Meowscript.Parser.RunParser
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Control.Monad.State (runStateT, liftIO, get, put)
import Control.Monad.Except (runExceptT, throwError)

runEvaluator :: EnvStack -> Evaluator a -> IO (Either Text.Text (a, EnvStack))
runEvaluator eval env = runExceptT (runStateT env eval)

runMeow :: EnvStack -> FilePath -> IO (Either Text.Text (Prim, EnvStack))
runMeow env path = meowParse path >>= \case
    (Left exception) -> (return . Left) exception
    (Right program) -> (runEvaluator env' . runBlock) program
    where env' = baseLibrary ++ env

runExpr :: EnvStack -> FilePath -> IO (Either Text.Text (Prim, EnvStack))
runExpr env path = exprParse path >>= \case
    (Left exception) -> (return . Left) exception
    (Right expr) -> (runEvaluator env' . evaluate) expr
    where env' = baseLibrary ++ env

-- No additional library variants:
runMeow' :: FilePath -> IO (Either Text.Text (Prim, EnvStack))
runMeow' = runMeow []

runExpr' :: FilePath -> IO (Either Text.Text (Prim, EnvStack))
runExpr' = runExpr []

-- Import a module:
importModule :: FilePath -> IO (Either Text.Text EnvStack)
importModule path = runMeow' path >>= \case
    (Left exception) -> (return . Left) exception
    (Right output) -> (return . Right . snd) output

addModule :: FilePath -> Evaluator ()
addModule path = (liftIO . importModule) path >>= \case
    (Left exception) -> throwError exception
    (Right moduleEnv) -> get >>= put . (++ moduleEnv)
