{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Meowr.RunMeowr
( runMeowr 
) where

import Meowscript.Meowr.Core
import Meowscript.Meowr.Parser
import Meowscript.Meowr.Project
import Meowscript.Core.AST
import Meowscript.Core.MeowState (meowState')
import Meowscript.Core.RunEvaluator (runMeow, importEnv)
import Meowscript.Core.Environment
import Meowscript.REPL.Loop (repl)
import Meowscript.API.JSON (toJSON, prettyJSON)
import Meowscript.Utils.IO
import Meowscript.Utils.Types
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Lens.Micro.Platform (over)
import Control.Monad ((>=>), void)
import Control.Monad.Except (ExceptT, runExceptT, liftIO, throwError)
import Data.IORef (readIORef)

{- Args -}
--------------------------------------------------
argStr :: IO Text.Text
argStr = Text.intercalate " " . map Text.pack <$> getArgs

getMeowr :: IO (Either Text.Text MeowrAction)
getMeowr = parseMeowed <$> argStr

applyArgs :: [MeowrArg] -> MeowState -> MeowState
applyArgs xs state = foldr runArg state xs

runArg :: MeowrArg -> MeowState -> MeowState
runArg (MeowrFlag flag)        = addFlag flag
runArg (MeowrString arg)       = addArg arg
runArg (MeowrOption key value) = addOption key value

transState :: [MeowrArg] -> MeowState -> MeowState
transState = (over meowArgs reverse .) . applyArgs

syntaxError :: Text.Text -> Text.Text
syntaxError = Text.append "Syntax error in command:\n"

{- Meowr -}
--------------------------------------------------
type Name = Text.Text
type Action = Name -> [MeowrArg] -> IO ()

meowrActions :: Map.Map Name Action
meowrActions = Map.fromList
    [ ("repl"   ,   (const . const) repl )
    , ("run"    ,   meowrMake none       )
    , ("json"   ,   meowrMake json       )
    , ("jsonp"  ,   meowrMake jsonP      )
    , ("proj"   ,   project              ) ]

{- A few notes:
 - 1. The 'json' and 'jsonp' commands should try to use a socket.
 - 2. The 'run' command works as you'd expect, following this rule:
 - 'meowr main.meows' == 'meowr run main.meows'
 - 3. All arguments passed to 'proj' will be passed only to 'config.meows'.
 - Not to the project itself! -}

runMeowr :: IO ()
runMeowr = getMeowr >>= \case
    (Left err) -> (printErrLn . syntaxError) err
    (Right (MeowrAction name args)) -> case name of
        Nothing -> repl -- todo: apply args here too
        (Just n) -> runAction n args

{- Run Action -}
--------------------------------------------------
takesName :: Set.Set Text.Text
takesName = Set.fromList ["run", "json", "jsonp"]

runAction :: Name -> [MeowrArg] -> IO ()
runAction name args = case Map.lookup name meowrActions of
    Nothing -> meowrMake none name args
    (Just f) -> if Set.member name takesName
        then f newName newArgs
        else f Text.empty args
    where (newName, newArgs) = case List.partition isMeowrStr args of
              ([], rest)    -> (Text.empty, rest)
              (x:xs, rest)  -> (getMeowrStr x, xs ++ rest)

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
project _ args = runExceptT (runProject args) >>= \case
    (Left exception) -> printExc exception
    (Right _) -> return ()

runProject  :: [MeowrArg] -> Project Prim
runProject args = do
    state <- liftIO $ meowState' configMeows' [] emptyLib
    let initState = transState args state

    configEnv <- readConfig initState
    configs <- liftIO $ applyConfigs initConfig configEnv

    options <- parseOptions (_configFlags configs)
    let finalState = transState options $ configState configs initState

    liftIO (runMeow finalState) >>= \case
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
    (Left err) -> throwError $ Text.append optionError err
    (Right options) -> (return . meowrArgs) options
    where optionError = Text.concat [ configMeows', ": Error when parsing options:\n" ]
