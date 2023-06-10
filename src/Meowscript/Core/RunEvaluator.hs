{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.RunEvaluator
( runMeow
, runFile
, runLine
, runCore
, runExpr
, runImport
, meowState
, MeowParams(..)
, MeowFile
, EvalCallback
, getImport
, importEnv
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
import Meowscript.Utils.Types
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Monad.Reader (asks, runReaderT, liftIO)
import Control.Monad.Except (runExceptT, throwError)
import Data.IORef (newIORef)
import Meowscript.Utils.IO

type EvalCallback a b = a -> Evaluator b
type MeowFile = Either Text.Text Text.Text

data MeowParams a b = MeowParams
    { getMeowState :: MeowState
    , getMeowFn    :: EvalCallback a b }

meowState :: [Text.Text] -> IO ObjectMap -> MeowState
meowState args lib = MeowState
    { meowArgs = args
    , meowLib = lib
    , meowStd = stdFiles }

runEvaluator :: MeowState -> IO ObjectMap -> Evaluator a -> IO (Either CatException a)
runEvaluator meow env eval = env >>= newIORef >>= (runExceptT . runReaderT eval . (meow,))

runFile :: MeowParams [Statement] b -> FilePath -> IO (Either CatException b)
runFile params path = safeReadFile path >>= runFileCore params path

-- Evaluate the contents from a .meows file.
runFileCore :: MeowParams [Statement] b -> FilePath -> MeowFile -> IO (Either CatException b)
runFileCore params path input = case input of
    (Left exception) -> (return . Left) $ badFile "In import" (Text.pack path) exception
    (Right contents) -> meowParse path contents >>= \case
        (Left exception) -> (return . Left) $ meowSyntaxExc exception
        (Right program) -> runCore state lib (asMain . fn) program
    where state = getMeowState params
          lib = meowLib state
          fn = getMeowFn params

-- Evaluate a single line. It's gonna be used in the REPL!
runLine :: Parser a -> MeowParams a b -> Text.Text -> IO (Either CatException b)
runLine parser params str = case parseSpecial parser str of
    (Left exception) -> (return . Left) $ meowSyntaxExc exception
    (Right output) -> runCore state lib fn output
    where state = getMeowState params
          lib = meowLib state
          fn = getMeowFn params

runCore :: MeowState -> IO ObjectMap -> EvalCallback a b -> a -> IO (Either CatException b)
runCore state lib fn input = do
    env <- (<>) <$> lib <*> baseLibrary
    (runEvaluator state (return env) . fn) input

--------------------------------------------------------------

-- Variants that default to no additional libraries:
runMeow :: FilePath -> IO (Either CatException Text.Text)
runMeow = runFile MeowParams
    { getMeowState = meowState [] (return Map.empty)
    , getMeowFn = runProgram' }

runExpr :: Text.Text -> IO (Either CatException Prim)
runExpr = runLine (lexemeLn parseExpr') MeowParams
    { getMeowState = meowState [] (return Map.empty)
    , getMeowFn = evaluate }

--------------------------------------------------------------

{- Stack trace. -}
asMain :: Evaluator a -> Evaluator a
asMain = stackTrace (return "In <main>.")

asImport :: FilePathT -> Evaluator a -> Evaluator a
asImport path = stackTrace (return $ Text.concat [ "In import: ", showT path ])

{- Run program with imports. -}
runProgram :: [Statement] -> Evaluator Prim
runProgram xs = do
    let (imps, rest) = List.partition isImport xs
    mapM_ addImport imps
    returnAsPrim <$> runBlock rest False

runProgram' :: [Statement] -> Evaluator Text.Text
runProgram' xs = runProgram xs >>= prettyMeow

runImport :: FilePathT -> [Statement] -> Evaluator Environment
runImport path xs = asImport path $ runProgram xs >> asks snd -- Return the environment.

{-
runDebug :: [Statement] -> Evaluator Text.Text
runDebug xs = do
    ret <- runProgram xs >>= prettyMeow
    x <- (asks snd >>= readMeowRef) >>= showMeow . MeowObject
    (liftIO . TextIO.putStrLn) x
    return ret
-}


{- Modules -}
--------------------------------------------------------

readModule :: FilePathT -> Evaluator MeowFile
readModule path = asks (meowStd . fst) >>= \x -> if Set.member path x
        then (liftIO . readStdFile) path
        else (liftIO . safeReadFile . Text.unpack) path

getImport :: FilePathT -> Evaluator (Either CatException Environment)
getImport path = do 
    x <- readModule path
    state <- asks fst
    liftIO (importEnv state path x)

importEnv :: MeowState -> FilePathT -> MeowFile -> IO (Either CatException Environment)
importEnv state path x = runFileCore params (Text.unpack path) x >>= \case
        (Left exception) -> (return . Left) exception
        (Right output) -> (return . Right) output
    where params = MeowParams
                { getMeowState = state
                , getMeowFn = runImport path }
    
addImport :: Statement -> Evaluator ()
addImport (StmImport filepath qualified) = getImport filepath >>= \case
    (Left ex) -> throwError ex
    (Right import') -> case qualified of
        Nothing -> do
            x <- asks snd >>= readMeowRef
            y <- readMeowRef import'
            asks snd >>= flip writeMeowRef (x <> y)
        (Just x) -> do
            imp <- readMeowRef import' >>= liftIO . newIORef . MeowObject
            insertRef x imp
addImport x = throwError $ notImport (showT x)
