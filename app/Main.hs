module Main (main) where

import Mewlix.CLI.Run (run)
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 run
