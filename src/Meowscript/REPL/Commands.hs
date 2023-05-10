{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.REPL.Commands
( LineCommand(..)
, Line(..)
, REPL
, Command
, IOCallback
, CommandMap
, runREPL
, commands
) where

import Meowscript.Core.AST
import Meowscript.Core.Pretty
import Meowscript.Core.RunEvaluator 
import Meowscript.Core.Keys
import Meowscript.Core.Blocks (evaluate)
import Meowscript.REPL.Utils
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
import Control.Monad (void, join)
import Data.IORef
import Data.Functor((<&>))
import System.IO (hFlush, stdout)
import Control.Monad.Reader (ReaderT, runReaderT)

data LineCommand = LineCommand
    { getName :: Text.Text
    , getArgs :: [Text.Text] }

data Line = Meta LineCommand | Expression Expr
type REPL a = ReaderT CommandMap IO a

type Command = LineCommand -> ObjectMap -> IOCallback -> IO ()
type IOCallback = ObjectMap -> IO ()
type CommandMap = Map.Map Text.Text Command

runREPL :: REPL a -> IO a
runREPL repl = runReaderT repl commands

commands :: CommandMap
commands = Map.fromList
    [ ("quit" , quit      )
    , ("load" , addModule ) ]

quit :: Command
quit _ _ _ = return ()

addModule :: Command
addModule line env fn = case getArgs line of
    [] -> fn env
    (x:_) -> getImportEnv (Text.unpack x) >>= \case
        (Left x') -> printStrLn x'
        (Right x') -> do
            env' <- readIORef x'
            let newEnv = env <> env'
            addModule (popArg line) newEnv fn

popArg :: LineCommand -> LineCommand
popArg l@(LineCommand _ []) = l
popArg (LineCommand name (_:xs)) = LineCommand name xs
