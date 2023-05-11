{-# LANGUAGE OverloadedStrings #-}

module Meowscript.REPL.Loop
( repl
) where

import Meowscript.Core.AST
import Meowscript.REPL.Core
import Meowscript.Core.Base
import Meowscript.REPL.Utils
import Meowscript.REPL.RunLine
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
import Control.Monad (when)
import Control.Monad.Reader (ask, liftIO)
import System.IO (hFlush, stdout)

replPrint :: Text.Text -> REPL ()
replPrint = liftIO . printStrLn

repl :: IO ()
repl = runREPL startRepl

mainLoop :: ObjectMap -> REPL ()
mainLoop env = do
    liftIO $ printStr "( ^.x.^)> :: " 
    (ret, env') <- takeLine env =<< liftIO TextIO.getLine
    when ret (mainLoop env')

startRepl :: REPL ()
startRepl = do
    replPrint "-------------------------------"
    replPrint "Welcome to the Meowscript REPL!"
    replPrint "-------------------------------"
    liftIO $ hFlush stdout
    liftIO baseLibrary >>= mainLoop
