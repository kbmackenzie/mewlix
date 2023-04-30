{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Meowscript (runBasic)

main :: IO ()
main = do
    let path = "meowscript_fibonacci.txt"
    tok <- runBasic path
    print tok
