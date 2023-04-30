{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Meowscript (runBasic)
import qualified Data.Text.IO as TextIO

main :: IO ()
main = do
    let path = "meowscript_fibonacci.txt"
    tok <- runBasic path
    case tok of
            (Right x) -> print x
            (Left x) -> TextIO.putStr x

