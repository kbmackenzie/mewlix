{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Meowscript (runBasic)
import qualified Data.Text.IO as TextIO

main :: IO ()
main = do
    --let path = "meowscript_practice6.txt"
    let path = "naive_fibonacci.txt"
    runBasic path 
