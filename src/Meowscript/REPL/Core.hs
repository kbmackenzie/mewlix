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
import Meowscript.Core.StdFiles
import Meowscript.Core.RunEvaluator 
import Meowscript.Utils.IO
import Meowscript.Utils.Data
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.IORef (readIORef)
import Data.Functor((<&>))

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
    (x:_) -> readModule path >>= importEnv state path >>= \case
        (Left x') -> printError x' >> return (True, env)
        (Right x') -> do
            env' <- readIORef x'
            let newEnv = env <> env'
            addModule (popArg line) newEnv
        where path = Text.unpack x
              state = meowState [] (return Map.empty)

readModule :: FilePath -> IO (Either Text.Text Text.Text)
readModule path = if Set.member path' stdFiles
    then readStdFile path'
    else safeReadFile path
    where path' = Text.pack path

popArg :: LineCommand -> LineCommand
popArg l@(LineCommand _ []) = l
popArg (LineCommand name (_:xs)) = LineCommand name xs

helpMessage :: [Text.Text]
helpMessage =
    [ "\n-- ~( ^.x.^)> --\n"
    , "Welcome to the Meowscript REPL!\n"
    , "You can use the following commands to navigate the REPL:"
    , ":help -> Show 'help' message. (You're here!)"
    , ":load -> Load a yarn ball into the REPL."
    , ":quit -> Quit the REPL."
    , "\n-- <(^.x.^ )~ --\n" ]

showHelp :: Command
showHelp _ env = printStrLn (Text.intercalate "\n" helpMessage) >> return (True, env)
