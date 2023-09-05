{-# LANGUAGE LambdaCase #-}

module Meowscript.REPL.Read
( runGetInput
) where

import Meowscript.Abstract.Meow (BoxMap)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import System.Console.Haskeline

type Input a = InputT IO a

keySearch :: BoxMap -> String -> [Completion]
keySearch env str = map (simpleCompletion . Text.unpack) matches
    where keys = HashMap.keys env
          packed = Text.pack str
          matches = filter (Text.isPrefixOf packed) keys

keyComplete :: BoxMap -> CompletionFunc IO
keyComplete env = completeWord Nothing [' ', '\t'] f
    where f = return . keySearch env

replSettings :: BoxMap -> Settings IO
replSettings env = setComplete (keyComplete env) defaultSettings

promptStr :: String
promptStr = "( ^.x.^)> :: "

readInput :: Input String
readInput = getInputLine promptStr >>= \case
    Nothing     -> readInput
    (Just x)    -> return x

runGetInput :: BoxMap -> IO String
runGetInput env = runInputT (replSettings env) readInput
