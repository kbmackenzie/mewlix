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
type MeowFile = Either Text.Text (MeowState, Text.Text)

data MeowParams a b = MeowParams
    { getMeowState :: MeowState
    , getMeowFn    :: EvalCallback a b }

{- Run evaluator: -}
-------------------------------------------------------------------------
runEvaluator :: MeowState -> IO ObjectMap -> Evaluator a -> IO (Either CatException a)
runEvaluator meow env eval = env >>= newIORef >>= (runExceptT . runReaderT eval . (meow,))

runFile :: MeowParams [Statement] b -> IO (Either CatException b)
runFile params = meowSearch state path >>= runFileCore params
    where state = getMeowState params
          path = (meowResolve state . Text.unpack . _meowPath) state

-- Evaluate the contents from a .meows file.
runFileCore :: MeowParams [Statement] b -> MeowFile -> IO (Either CatException b)
{-# INLINABLE runFileCore #-}
runFileCore params input = case input of
    (Left exception) -> (return . Left) $ badFile "In import" (Text.pack path) exception
    (Right (newState, contents)) -> meowParse path contents >>= do
        let newParams = params { getMeowState = newState }
        runParsed newParams
    where state = getMeowState params
          path = (meowResolve state . Text.unpack . _meowPath) state

runParsed :: MeowParams [Statement] b -> Either Text.Text [Statement] -> IO (Either CatException b)
{-# INLINABLE runParsed #-}
runParsed params parsed = case parsed of
    (Left exception) -> (return . Left) $ meowSyntaxExc exception
    (Right program) -> runCore state lib (asMain . fn) program
    where state = getMeowState params
          lib = _meowLib state
          fn = getMeowFn params

runCore :: MeowState -> IO ObjectMap -> EvalCallback a b -> a -> IO (Either CatException b)
{-# INLINABLE runCore #-}
runCore state lib fn input = do
    env <- (<>) <$> lib <*> baseLibrary
    (runEvaluator state (return env) . fn) input

{- Helper functions for running script files: -}
-------------------------------------------------------------------------
-- Variants that default to no additional libraries:
runMeow :: MeowState -> IO (Either CatException Prim)
runMeow state = runFile MeowParams
    { getMeowState = state
    , getMeowFn = runProgram }

runExpr :: Text.Text -> IO (Either CatException Prim)
runExpr line = meowState' Text.empty [] emptyLib >>= \state -> 
    let params = MeowParams
            { getMeowState = state
            , getMeowFn = evaluate }
    in runLine (lexemeLn parseExpr') params line

{- More actions -}
-------------------------------------------------------------------------
-- Evaluate a single line.
runLine :: Parser a -> MeowParams a b -> Text.Text -> IO (Either CatException b)
runLine parser params str = case parseSpecial parser str of
    (Left exception) -> (return . Left) $ meowSyntaxExc exception
    (Right output) -> runCore state lib fn output
    where state = getMeowState params
          lib = _meowLib state
          fn = getMeowFn params

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

runImport :: FilePathT -> [Statement] -> Evaluator Environment
runImport path xs = asImport path $ runProgram xs >> asks snd -- Return the environment.


{- Modules -}
--------------------------------------------------------
readModule :: FilePathT -> Evaluator MeowFile
readModule path = asks (_meowStd . fst) >>= \std -> if Set.member path std
    then do
        output <- (liftIO . readStdFile) path
        state <- asks fst
        return $ fmap (state,) output
    else do
        local <- asks $ (`localPath` Text.unpack path) . Text.unpack . _meowPath . fst
        state <- asks fst
        (liftIO . meowSearch state . meowResolve state) local

getImport :: FilePathT -> Evaluator (Either CatException Environment)
{-# INLINABLE getImport #-}
getImport path = cacheLookup path >>= \case
    (Just x) -> return (Right x)
    Nothing  -> do
        contents <- readModule path
        state <- asks fst
        liftIO (importEnv state path contents)

--importLog :: FilePathT -> IO () -- todo: take this out as it's just for info
--importLog = putStrLn . ("Importing... " ++) . Text.unpack

importEnv :: MeowState -> FilePathT -> MeowFile -> IO (Either CatException Environment)
{-# INLINABLE importEnv #-}
importEnv state path x = runFileCore params x >>= \case
        (Left exception) -> (return . Left) exception
        (Right output) -> (return . Right) output
    where params = MeowParams
                { getMeowState = meowSetPath path state
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
            lib <- asks (_meowLib . fst) >>= liftIO
            asks snd >>= flip writeMeowRef (x <> y <> lib)
        (Just name) -> do
            x <- publicKeys <$> readMeowRef imp 
            lib <- asks (_meowLib . fst) >>= liftIO
            insertRef name =<< (newMeowRef . MeowObject) (x <> lib)
addImport x = throwError $ notImport (showT x)
