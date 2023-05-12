{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Core.RunEvaluator
( runMeow
, runFile
, runLine
, runCore
, runExpr
, runMeowDebug
, getImportEnv
, EvalCallback
) where

import Meowscript.Core.AST
import Meowscript.Core.Base
import Meowscript.Core.Environment
import Meowscript.Core.Blocks
import Meowscript.Parser.Expr (parseExpr')
import Meowscript.Parser.Core (Parser, lexemeLn)
import Meowscript.Parser.RunParser
import Meowscript.Core.Exceptions
import Meowscript.Core.Pretty
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Reader (ask, runReaderT, liftIO)
import Control.Monad.Except (runExceptT, throwError)
import Data.IORef

type EvalCallback a b = a -> Evaluator b

runEvaluator :: IO ObjectMap -> Evaluator a -> IO (Either Text.Text a)
runEvaluator env eval = env >>= newIORef >>= (runExceptT . runReaderT eval)

-- Evaluate the contents from a .meows file.
runFile :: IO ObjectMap -> EvalCallback [Statement] b -> FilePath -> IO (Either Text.Text b)
runFile lib fn path = meowParse path >>= \case
    (Left exception) -> (return . Left) exception
    (Right program) -> runCore lib fn program

-- Evaluate a single line. It's gonna be used in the REPL!
runLine :: IO ObjectMap -> Parser a -> EvalCallback a b -> Text.Text -> IO (Either Text.Text b)
runLine lib parser fn str = case parseSpecial parser str of
    (Left exception) -> (return . Left) exception
    (Right output) -> runCore lib fn output

runCore :: IO ObjectMap -> EvalCallback a b -> a -> IO (Either Text.Text b)
runCore lib fn input = do
    env <- (<>) <$> lib <*> baseLibrary
    (runEvaluator (return env) . fn) input

--------------------------------------------------------------

-- Variants that default to no additional libraries:
runMeow :: FilePath -> IO (Either Text.Text Text.Text)
runMeow = runFile (return Map.empty) runProgram'

runExpr :: Text.Text -> IO (Either Text.Text Prim)
runExpr = runLine (return Map.empty) (lexemeLn parseExpr') evaluate

runMeowDebug :: FilePath -> IO (Either Text.Text Text.Text)
runMeowDebug = runFile (return Map.empty) runDebug

--------------------------------------------------------------

{- Run program with imports. -}
runProgram :: [Statement] -> Evaluator Prim
runProgram xs = do
    let (imps, rest) = List.partition isImport xs
    mapM_ addImport imps
    returnAsPrim <$> runBlock rest False

runProgram' :: [Statement] -> Evaluator Text.Text
runProgram' xs = runProgram xs >>= showMeow

runAsImport :: [Statement] -> Evaluator Environment
runAsImport xs = runProgram xs >> ask -- Return the environment.

runDebug :: [Statement] -> Evaluator Text.Text
runDebug xs = do
    ret <- runProgram xs >>= showMeow
    x <- (ask >>= liftIO . readIORef) >>= showMeow . MeowObject
    (liftIO . TextIO.putStrLn) x
    return ret

getImportEnv :: FilePath -> IO (Either Text.Text Environment)
getImportEnv path = runFile (return Map.empty) runAsImport path >>= \case
    (Left exception) -> (return . Left) exception
    (Right output) -> (return . Right) output

addImport :: Statement -> Evaluator ()
addImport (StmImport file qualified) = (liftIO . getImportEnv) file >>= \case
    (Left ex) -> throwError (badImport file ex)
    (Right import') -> case qualified of
        Nothing -> do
            x <- ask >>= liftIO . readIORef
            y <- (liftIO . readIORef) import'
            ask >>= liftIO . flip writeIORef (x <> y)
        (Just x) -> do
            imp <- (liftIO . readIORef) import' >>= liftIO . newIORef . MeowObject
            insertRef x imp
addImport x = throwError (showException MeowBadImport
    "Critical error: Statement is not an import! Trace: " `Text.append` showT x)
