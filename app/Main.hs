{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Meowscript (runBasic)
import qualified Data.Text.IO as TextIO

main :: IO ()
main = do
    --let path = "meowscript_practice6.txt"
    --let path = "naive_fibonacci.txt"
    let path = "stdmeows_draft1.txt"
    runBasic path 
