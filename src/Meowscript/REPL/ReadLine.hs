{-# LANGUAGE LambdaCase #-}

module Meowscript.REPL.ReadLine
( Input
, keyComplete
, replSettings
, readInput
, runGetInput
) where

import Meowscript.Core.AST
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import System.Console.Haskeline

type Input a = InputT IO a

keySearch :: ObjectMap -> String -> [Completion]
keySearch env str = map (simpleCompletion . Text.unpack) matches
    where
        keys = Map.keys env
        str' = Text.pack str
        matches = filter (Text.isPrefixOf str') keys

keyComplete :: ObjectMap -> CompletionFunc IO
keyComplete env = completeWord Nothing [' ', '\t'] f
    where f = return . keySearch env

replSettings :: ObjectMap -> Settings IO
replSettings env = setComplete (keyComplete env) defaultSettings

promptStr :: String
promptStr = "( ^.x.^)> :: "

readInput :: Input String
readInput = getInputLine promptStr >>= \case
    Nothing     -> readInput
    (Just x)    -> return x

runGetInput :: ObjectMap -> IO String
runGetInput env = runInputT (replSettings env) readInput
