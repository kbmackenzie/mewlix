{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.REPL.Loop
( mainLoop
, startRepl
) where

import Meowscript.Core.AST
import Meowscript.Core.Pretty
import Meowscript.Core.RunEvaluator 
import Meowscript.Core.Keys
import Meowscript.Core.Blocks (evaluate)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
import Control.Monad (void, join)
import Control.Monad.Reader (ask)
import Data.IORef
import Data.Functor((<&>))
import System.IO (hFlush, stdout)

exprAndEnv :: ExprCallback (Prim, Environment)
exprAndEnv x = (,) <$> (evaluate x >>= ensureValue) <*> ask

runExpression :: ObjectMap -> Text.Text -> IO (Either Text.Text (Prim, Environment))
runExpression env = runExpr (return env) exprAndEnv

exprLoop :: ObjectMap -> IO (Text.Text, ObjectMap)
exprLoop env = TextIO.getLine >>= runExpression env >>= \case
    (Left x) -> return (x, env)
    (Right (x, y)) -> do
        env' <- readIORef y
        let newEnv = env <> env'
        return (showT x, newEnv)

-- Flush stdout after printing so that it happens before getLine.
printLine :: Text.Text -> IO ()
printLine line = TextIO.putStr line >> hFlush stdout

mainLoop :: ObjectMap -> IO ()
mainLoop env = do
    printLine "( ^.x.^)> :: " 
    (ret, env') <- exprLoop env
    TextIO.putStrLn ret
    mainLoop env'

startRepl :: IO ()
startRepl = do
    TextIO.putStrLn "Welcome to the Meowscript REPL!"
    TextIO.putStrLn "-------------------------------"
    hFlush stdout
    mainLoop Map.empty
