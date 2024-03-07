{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Test.Compiler (compileFile)
import System.Environment (getArgs)
import qualified Data.Text.IO as TextIO

-- Compile a yarn ball containing every expression, operator and statement found in Mewlix.
-- Run after making any sort of change to the parser and compiler.
syntax :: IO ()
syntax = compileFile path >>= TextIO.putStrLn
    where path = "test/language.mews"

main :: IO ()
main = getArgs >>= \case
    []          -> syntax
    (path : _)  -> compileFile path >>= TextIO.putStrLn
