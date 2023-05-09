{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Meowscript (runBasic)
import qualified Data.Text.IO as TextIO

main :: IO ()
main = do
    let path = "meowscript_practice3.txt"
    return ()
    runBasic path 
