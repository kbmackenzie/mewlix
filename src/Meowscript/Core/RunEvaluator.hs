{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.RunEvaluator
( runMeow
, runFile
, runLine
, runCore
, runExpr
, runMeowDebug
, getImportEnv
, getImportEnv'
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
import Meowscript.Core.StdFiles
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Monad.Reader (asks, runReaderT, liftIO)
import Control.Monad.Except (runExceptT, throwError)
import Data.IORef (newIORef)
import Meowscript.Utils.IO

type EvalCallback a b = a -> Evaluator b

meowState :: [Text.Text] -> MeowState
meowState args = MeowState
    { meowArgs = args
    , meowStd = stdFiles }

runEvaluator :: MeowState -> IO ObjectMap -> Evaluator a -> IO (Either Text.Text a)
runEvaluator meow env eval = env >>= newIORef >>= (runExceptT . runReaderT eval . (meow,))

runFile :: IO ObjectMap -> EvalCallback [Statement] b -> FilePath -> IO (Either Text.Text b)
runFile lib fn path = safeReadFile path >>= runFileCore lib fn path

-- Evaluate the contents from a .meows file.
runFileCore :: IO ObjectMap -> EvalCallback [Statement] b -> FilePath -> Either Text.Text Text.Text -> IO (Either Text.Text b)
runFileCore lib fn path input = case input of
    (Left exception) -> (return . Left) exception
    (Right contents) -> meowParse path contents >>= \case
        (Left exception) -> (return . Left) exception
        (Right program) -> runCore lib (asMain . fn) program

-- Evaluate a single line. It's gonna be used in the REPL!
runLine :: IO ObjectMap -> Parser a -> EvalCallback a b -> Text.Text -> IO (Either Text.Text b)
runLine lib parser fn str = case parseSpecial parser str of
    (Left exception) -> (return . Left) exception
    (Right output) -> runCore lib fn output

runCore :: IO ObjectMap -> EvalCallback a b -> a -> IO (Either Text.Text b)
runCore lib fn input = do
    env <- (<>) <$> lib <*> baseLibrary
    (runEvaluator (meowState []) (return env) . fn) input

--------------------------------------------------------------

-- Variants that default to no additional libraries:
runMeow :: FilePath -> IO (Either Text.Text Text.Text)
runMeow = runFile (return Map.empty) runProgram'

runExpr :: Text.Text -> IO (Either Text.Text Prim)
runExpr = runLine (return Map.empty) (lexemeLn parseExpr') evaluate

runMeowDebug :: FilePath -> IO (Either Text.Text Text.Text)
runMeowDebug = runFile (return Map.empty) runDebug

--------------------------------------------------------------

{- Stack trace. -}
asMain :: Evaluator a -> Evaluator a
asMain = stackTrace (return "In <main>.")

asImport :: FilePath -> Evaluator a -> Evaluator a
asImport path = stackTrace (return $ Text.concat [ "In import: ", showT path ])

{- Run program with imports. -}
runProgram :: [Statement] -> Evaluator Prim
runProgram xs = do
    let (imps, rest) = List.partition isImport xs
    mapM_ addImport imps
    returnAsPrim <$> runBlock rest False

runProgram' :: [Statement] -> Evaluator Text.Text
runProgram' xs = runProgram xs >>= prettyMeow

runImport :: FilePath -> [Statement] -> Evaluator Environment
runImport path xs = asImport path $ runProgram xs >> asks snd -- Return the environment.

runDebug :: [Statement] -> Evaluator Text.Text
runDebug xs = do
    ret <- runProgram xs >>= prettyMeow
    x <- (asks snd >>= readMeowRef) >>= showMeow . MeowObject
    (liftIO . TextIO.putStrLn) x
    return ret


{- Modules -}
--------------------------------------------------------

readModule :: FilePath -> Evaluator (Either Text.Text Text.Text)
readModule path = asks (meowStd . fst) >>= \x -> if Set.member path' x
        then (liftIO . readStdFile) path'
        else (liftIO . safeReadFile) path
    where path' = Text.pack path

getImportEnv :: FilePath -> Evaluator (Either Text.Text Environment)
getImportEnv path = do 
    x <- readModule path
    liftIO (getImportEnv' path x)

getImportEnv' :: FilePath -> Either Text.Text Text.Text -> IO (Either Text.Text Environment)
getImportEnv' path x = runFileCore (return Map.empty) (runImport path) path x >>= \case
        (Left exception) -> (return . Left) exception
        (Right output) -> (return . Right) output
    
addImport :: Statement -> Evaluator ()
addImport (StmImport file qualified) = getImportEnv file >>= \case
    (Left ex) -> throwError (badImport file ex)
    (Right import') -> case qualified of
        Nothing -> do
            x <- asks snd >>= readMeowRef
            y <- readMeowRef import'
            asks snd >>= flip writeMeowRef (x <> y)
        (Just x) -> do
            imp <- readMeowRef import' >>= liftIO . newIORef . MeowObject
            insertRef x imp
addImport x = throwError (showException MeowBadImport
    "Critical error: Statement is not an import! Trace: " `Text.append` showT x)
