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
, MeowParams(..)
, MeowFile
, EvalCallback
, getImport
, importEnv
, publicKeys
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
import Meowscript.Core.MeowState
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

{- Run evaluator: -}
-------------------------------------------------------------------------
runEvaluator :: MeowState -> IO ObjectMap -> Evaluator a -> IO (Either CatException a)
runEvaluator meow env eval = env >>= newIORef >>= (runExceptT . runReaderT eval . (meow,))

runFile :: MeowParams [Statement] b -> IO (Either CatException b)
runFile params = safeReadFile path >>= runFileCore params
    where path = (Text.unpack . meowPath . getMeowState) params

-- Evaluate the contents from a .meows file.
runFileCore :: MeowParams [Statement] b -> MeowFile -> IO (Either CatException b)
runFileCore params input = case input of
    (Left exception) -> (return . Left) $ badFile "In import" (Text.pack path) exception
    (Right contents) -> meowParse path contents >>= \case
        (Left exception) -> (return . Left) $ meowSyntaxExc exception
        (Right program) -> runCore state lib (asMain . fn) program
    where state = getMeowState params
          lib = meowLib state
          fn = getMeowFn params
          path = (Text.unpack . meowPath . getMeowState) params

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


{- Helper functions for running script files: -}
-------------------------------------------------------------------------
-- Variants that default to no additional libraries:
runMeow :: FilePathT -> IO (Either CatException Text.Text)
runMeow path = meowState' path [] emptyLib >>= \state -> runFile MeowParams
    { getMeowState = state
    , getMeowFn = runProgram' }

runExpr :: Text.Text -> IO (Either CatException Prim)
runExpr line = meowState' Text.empty [] emptyLib >>= \state -> 
    let params = MeowParams
            { getMeowState = state
            , getMeowFn = evaluate }
    in runLine (lexemeLn parseExpr') params line


{- Stack tracing: -}
-------------------------------------------------------------------------
asMain :: Evaluator a -> Evaluator a
asMain = stackTrace (return "In <main>.")

asImport :: FilePathT -> Evaluator a -> Evaluator a
asImport path = stackTrace (return $ Text.concat [ "In import: ", showT path ])


{- Run program (and imports): -}
-------------------------------------------------------------------------
runProgram :: [Statement] -> Evaluator Prim
runProgram xs = do
    let (imps, rest) = List.partition isImport xs
    mapM_ addImport imps
    returnAsPrim <$> runBlock rest False

runProgram' :: [Statement] -> Evaluator Text.Text
runProgram' xs = runProgram xs >>= prettyMeow

runImport :: FilePathT -> [Statement] -> Evaluator Environment
runImport path xs = asImport path $ runProgram xs >> asks snd -- Return the environment.


{- Modules -}
--------------------------------------------------------
readModule :: FilePathT -> Evaluator MeowFile
readModule path = asks (meowStd . fst) >>= \std -> if Set.member path std
    then (liftIO . readStdFile) path
    else do
        local <- asks (localPath . meowPath . fst)
        (liftIO . safeReadFile . Text.unpack) (local path)

getImport :: FilePathT -> Evaluator (Either CatException Environment)
{-# INLINABLE getImport #-}
getImport path = cacheLookup path >>= \case
    (Just x) -> return (Right x)
    Nothing  -> do
        contents <- readModule path
        state <- asks fst
        liftIO (importEnv state path contents)

importLog :: FilePathT -> IO () -- todo: take this out as it's just for info
importLog = putStrLn . ("Importing... " ++) . Text.unpack

importEnv :: MeowState -> FilePathT -> MeowFile -> IO (Either CatException Environment)
{-# INLINABLE importEnv #-}
importEnv state path x = importLog path >> runFileCore params x >>= \case
        (Left exception) -> (return . Left) exception
        (Right output) -> (return . Right) output
    where params = MeowParams
                { getMeowState = meowNewPath state path
                , getMeowFn = runImport path }

publicKeys :: ObjectMap -> ObjectMap
{-# INLINABLE publicKeys #-}
publicKeys = Map.filterWithKey (\k _ -> (not . Text.isPrefixOf "_") k)
    
addImport :: Statement -> Evaluator ()
{-# INLINABLE addImport #-}
addImport (StmImport filepath qualified) = getImport filepath >>= \case
    (Left ex) -> throwError ex
    (Right imp) -> cacheAdd filepath imp
        >> case qualified of
        Nothing -> do
            x <- asks snd >>= readMeowRef
            y <- publicKeys <$> readMeowRef imp
            lib <- asks (meowLib . fst) >>= liftIO
            asks snd >>= flip writeMeowRef (x <> y <> lib)
        (Just name) -> do
            x <- publicKeys <$> readMeowRef imp 
            lib <- asks (meowLib . fst) >>= liftIO
            insertRef name =<< (newMeowRef . MeowObject) (x <> lib)
addImport x = throwError $ notImport (showT x)
