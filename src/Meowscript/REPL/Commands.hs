{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.REPL.Commands
( Command
, IOCallback
, CommandMap
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
import Control.Monad.Reader (ask)
import Data.IORef
import Data.Functor((<&>))
import System.IO (hFlush, stdout)

type Command = [Text.Text] -> ObjectMap -> IOCallback -> IO ()
type IOCallback = ObjectMap -> IO ()
type CommandMap = Map.Map Text.Text Command

commands :: CommandMap
commands = Map.fromList
    [ ("quit" , quit      )
    , ("load" , addModule ) ]

quit :: Command
quit _ _ _ = return ()

addModule :: Command
addModule [] env fn = fn env
addModule (x:xs) env fn = getImportEnv (Text.unpack x) >>= \case
    (Left x') -> printStrLn x'
    (Right x') -> do
        env' <- readIORef x'
        let newEnv = env <> env'
        addModule xs newEnv fn
