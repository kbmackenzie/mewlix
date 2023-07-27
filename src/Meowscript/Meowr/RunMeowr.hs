{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Meowr.RunMeowr
( runMeowr 
) where

import Meowscript.Core.AST
import Meowscript.Meowr.Core
import Meowscript.Meowr.Parser
import Meowscript.Core.MeowState (meowState')
import Meowscript.Core.RunEvaluator (runMeow, importEnv)
import Meowscript.Core.Environment
import Meowscript.Meowr.Project
import Meowscript.REPL.Loop (repl)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Lens.Micro.Platform (over)
import Meowscript.API.JSON (toJSON, prettyJSON)
import Control.Monad ((>=>), void)
import Control.Monad.Except (ExceptT, runExceptT, liftIO, throwError)
import Data.IORef (readIORef)
import Meowscript.Utils.IO
import Meowscript.Utils.Types

argStr :: IO Text.Text
argStr = Text.intercalate " " . map Text.pack <$> getArgs

getMeowr :: IO (Either Text.Text MeowrAction)
getMeowr = parseMeowed <$> argStr

applyArgs :: [MeowrArg] -> MeowState -> MeowState
applyArgs xs state = foldl (flip meowrAction) state xs

meowrAction :: MeowrArg -> MeowState -> MeowState
meowrAction (MeowrFlag flag) = addFlag flag
meowrAction (MeowrOption key value) = addOption key value
meowrAction (MeowrString arg) = addArg arg

transState :: [MeowrArg] -> MeowState -> MeowState
transState = (over meowArgs reverse .) . applyArgs

makeError :: Text.Text -> Text.Text
makeError = Text.append "Syntax error in command:\n"

type Name = Text.Text
type Action = Name -> [MeowrArg] -> IO ()

meowrActions :: Map.Map Name Action
meowrActions = Map.fromList
    [ ("repl"   ,   (const . const) repl )
    , ("run"    ,   meowrMake none       )
    , ("json"   ,   meowrMake json       )
    , ("jsonp"  ,   meowrMake jsonP      )
    , ("proj"   ,   project              ) ]

runMeowr :: IO ()
runMeowr = getMeowr >>= \case
    (Left err) -> (printErrLn . makeError) err
    (Right (MeowrAction name args)) -> case name of
        Nothing -> repl -- todo: apply args here too
        (Just n) -> runAction n args

getMeowrStr :: MeowrArg -> Text.Text
getMeowrStr (MeowrString x) = x
getMeowrStr _ = undefined -- This should never happen.

runAction :: Name -> [MeowrArg] -> IO ()
runAction name args = case Map.lookup name meowrActions of
    Nothing -> meowrMake none name args
    (Just f) -> f newName newArgs
    where (newName, newArgs) = case List.partition isMeowrStr args of
              ([], rest) -> ("", rest)
              (x:xs, rest) -> (getMeowrStr x, xs ++ rest)

none :: Prim -> IO ()
none = void . return

json :: Prim -> IO ()
json = toJSON >=> printStrLn

jsonP :: Prim -> IO ()
jsonP = prettyJSON >=> printStrLn

meowrMake :: (Prim -> IO ()) -> Name -> [MeowrArg] -> IO ()
meowrMake f name args = do
    state <- meowState' name [] emptyLib
    let meowedState = transState args state
    runMeow meowedState >>= \case
        (Left exc) -> (printExc . snd) exc
        (Right prim) -> f prim

{- Run Project -}
-------------------------------------------------
noConfig :: Text.Text -> Text.Text
noConfig = Text.append "Couldn't load 'config.meows' script: \n"

configMeows :: FilePath
configMeows = "config.meows"

configMeows' :: FilePathT
configMeows' = Text.pack configMeows

type Project a = ExceptT Text.Text IO a

project :: Name -> [MeowrArg] -> IO ()
project _ _ = runExceptT runProject >>= \case
    (Left exception) -> printExc exception
    (Right _) -> return ()

runProject  :: Project Prim
runProject = do
    state <- liftIO $ meowState' configMeows' [] emptyLib

    configEnv <- readConfig state
    configs <- liftIO $ applyConfigs initConfig configEnv

    let beforeOpts = configState configs state
    options <- parseOptions (_configFlags configs)
    let newState = transState options beforeOpts

    liftIO (runMeow newState) >>= \case
        (Left exception) -> (throwError . snd) exception
        (Right prim) -> return prim

readConfig :: MeowState -> Project ObjectMap
readConfig state = liftIO (safeDoesFileExist configMeows) >>= \case
    (Left exception) -> (throwError . noConfig) exception
    (Right False) -> (throwError . noConfig) "Script not found!"
    (Right True)  -> do
        contents <- fmap (state,) <$> liftIO (safeReadFile configMeows)
        liftIO (importEnv state configMeows' contents) >>= \case
            (Left exception) -> (throwError . snd) exception
            (Right env) -> liftIO (readIORef env)

parseOptions :: [Text.Text] -> Project [MeowrArg]
parseOptions opts = case parseMeowed (Text.intercalate " " opts) of
    (Left err) -> throwError $ Text.append "Error when passing 'options':\n" err
    (Right options) -> (return . meowrArgs) options
