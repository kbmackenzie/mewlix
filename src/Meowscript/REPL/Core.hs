{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import Meowscript.Core.Pretty
import Meowscript.Core.RunEvaluator 
--import Meowscript.Core.Keys
--import Meowscript.Core.Blocks (evaluate)
import Meowscript.Utils.IO
import qualified Data.Text as Text
--import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
--import Control.Monad (void, join)
import Data.IORef
--import Data.Functor((<&>))
--import System.IO (hFlush, stdout)
import Control.Monad.Reader (ReaderT, runReaderT)

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

commands :: CommandMap
commands = Map.fromList
    [ ("quit" , quit      )
    , ("help" , showHelp  )
    , ("load" , addModule )]

quit :: Command
quit _ env = return (False, env)

addModule :: Command
addModule line env = case getArgs line of
    [] -> return (True, env)
    (x:_) -> getImportEnv (Text.unpack x) >>= \case
        (Left x') -> printError x' >> return (True, env)
        (Right x') -> do
            env' <- readIORef x'
            let newEnv = env <> env'
            addModule (popArg line) newEnv

popArg :: LineCommand -> LineCommand
popArg l@(LineCommand _ []) = l
popArg (LineCommand name (_:xs)) = LineCommand name xs

helpMessage :: [Text.Text]
helpMessage =
    [ "-------------------------------"
    , "Welcome to the Meowscript REPL!"
    , "You can use the following commands to navigate the REPL:"
    , ":help -> Show 'help' message. (You're here!)"
    , ":load -> Load a module into the current context."
    , ":quit -> Quit the REPL."
    , "-------------------------------"]

showHelp :: Command
showHelp _ env = printStrLn (Text.intercalate "\n" helpMessage) >> return (True, env)
