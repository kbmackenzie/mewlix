{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.REPL.Core
( LineCommand(..)
, Line(..)
, REPL
, Command
, Continue
, CommandMap
, runREPL
, commands
) where

import Meowscript.Core.AST
import Meowscript.Core.StdFiles
import Meowscript.Core.Environment
import Meowscript.Core.RunEvaluator 
import Meowscript.Core.Base (baseLibrary)
import Meowscript.Utils.IO
import Meowscript.Utils.Show
import Meowscript.Utils.Types
import Meowscript.Core.MeowState
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.IORef (readIORef)

{-
 - -- REPL LOOP --
 -
 - 1. Main branches off into two paths:
 -   A. Line command
 -   B. Expression
 - 2. Main is passed as a callback to the function it calls.
 - 3. The function it called calls back to main.
 -
 -}

data LineCommand = LineCommand
    { getName :: Text.Text
    , getArgs :: [Text.Text] }

data Line = Meta LineCommand | Expression Expr
type REPL a = ReaderT CommandMap IO a

type Command = LineCommand -> ObjectMap -> IO (Continue, ObjectMap)
type Continue = Bool
type CommandMap = Map.Map Text.Text Command

runREPL :: REPL a -> IO a
runREPL repl = runReaderT repl commands

{- To Do: Add commands
 - ':inspect' -> Lists all keys (variables + functions) in the environment and their contents.
 - ':ask'     -> Lets you look up a regexp for a function or variable in scope and get all keys
 - that match that regexp, along with their contents.
 - ':clear'   -> Clears the environment.
 -}

commands :: CommandMap
commands = Map.fromList
    [ ("quit"    , quit       )
    , ("help"    , showHelp   )
    , ("load"    , addModule  )
    , ("inspect" , inspectEnv )
    , ("ask"     , askFuncEnv )
    , ("clear"   , clearEnv   )]

quit :: Command
quit _ env = return (False, env)

addModule :: Command
addModule line env = case getArgs line of
    [] -> return (True, env)
    (path:_) -> readModule path >>= \contents -> do
        state' <- makeState
        let contents' = fmap (state',) contents
        importEnv state' path contents' >>= \case
            (Left x) -> printExc (snd x) >> return (True, env)
            (Right x) -> do
                imp <- publicKeys <$> readIORef x
                let envNew = env <> imp
                addModule (popArg line) envNew
    where makeState = meowState' Text.empty [] emptyLib

readModule :: FilePathT -> IO (Either Text.Text Text.Text)
readModule path = if Set.member path stdFiles
    then readStdFile path
    else safeReadFile (Text.unpack path)

popArg :: LineCommand -> LineCommand
popArg l@(LineCommand _ []) = l
popArg (LineCommand name (_:xs)) = LineCommand name xs

helpMessage :: [Text.Text]
helpMessage =
    [ "\n-- ~( ^.x.^)> --\n"
    , "Welcome to the Meowscript REPL!\n"
    , "You can use the following commands to navigate the REPL:"
    , ":help    -> Show 'help' message. (You're here!)"
    , ":load    -> Load a yarn ball into the REPL."
    , ":ask     -> Look up a key in the current environment."
    , ":inspect -> View all keys in the current environment."
    , ":clear   -> Clear the current environment."
    , ":quit    -> Quit the REPL."
    , "\n-- <(^.x.^ )~ --\n" ]

showHelp :: Command
showHelp _ env = printStrLn (Text.intercalate "\n" helpMessage) >> return (True, env)

inspectEnv :: Command
inspectEnv _ env = do
    let prettyKey = flip Text.append ": "
    let prettyRef ref = showT <$> readIORef ref
    let pretty (key, ref) = (prettyKey key `Text.append`) <$> prettyRef ref
    printStrLn . Text.unlines =<< mapM pretty (Map.toList env)
    return (True, env)

askFuncEnv :: Command
askFuncEnv line env = case getArgs line of
    [] -> inspectEnv line env
    (x:_) -> do
        let prettyKey = flip Text.append ": "
        let prettyRef ref = showT <$> readIORef ref
        let pretty (key, ref) = (prettyKey key `Text.append`) <$> prettyRef ref
        let keysAskedFor = Map.filterWithKey (\k _ -> Text.isPrefixOf x k) env
        printStrLn . Text.unlines =<< mapM pretty (Map.toList keysAskedFor)
        return (True, env)

clearEnv :: Command
clearEnv _ _ = (True,) <$> baseLibrary
