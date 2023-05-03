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
import Meowscript.Core.Exceptions
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.State (runStateT, liftIO, get, put)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad (void)

runEvaluator :: EnvStack -> Evaluator a -> IO (Either Text.Text (a, EnvStack))
runEvaluator eval env = runExceptT (runStateT env eval)

runMeow :: EnvStack -> FilePath -> IO (Either Text.Text (Prim, EnvStack))
runMeow env path = meowParse path >>= \case
    (Left exception) -> (return . Left) exception
    (Right program) -> (runEvaluator env' . runProgram) program
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


{- Run program with imports. -}
runProgram :: [Statement] -> Evaluator Prim
runProgram xs = do
    let (imps, rest) = List.partition isImport xs
    mapM_ runImport imps
    runBlock rest

{- Imports -}
{- A few things about imports:
 - They shouldn't take the base library. That's already 'implied'.
 - They can be imported qualified or not. One is easier than the other. @x@ -}

getImportEnv :: FilePath -> IO (Either Text.Text EnvStack)
getImportEnv path = runMeow' path >>= \case
    (Left exception) -> (return . Left) exception
    (Right output) -> (return . Right . snd) output

{- The environment stack should contain only one element after
 - running a script either way, but I'm folding it into one to
 - be neat. -}
 
envStackFold :: EnvStack -> Environment
envStackFold = foldl (<>) Map.empty

{- Create a transformation function for the main environment,
 - taking into account whether the module is qualified. -}

addImport :: Environment -> Qualified -> (Environment -> Environment)
addImport imp qualified = case qualified of
    Nothing -> (<> imp)
    (Just x) -> let imp' = Map.fromList [(x, MeowObject imp)] 
                in (<> imp')

runImport :: Statement -> Evaluator ()
runImport (SImport file key) = (liftIO . getImportEnv) file >>= \case
    (Left ex) -> throwError (badImport file ex)
    (Right impEnv) -> do
        let imp = addImport (envStackFold impEnv) key
        (x:xs) <- get
        put (imp x:xs)
runImport _ = throwError (showException MeowBadImport "Critical error: Statement is not an import!")
