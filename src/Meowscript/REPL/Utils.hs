module Meowscript.REPL.Utils
( printStr
, printStrLn
) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO (hFlush, stdout)

printStr :: Text.Text -> IO ()
printStr line = TextIO.putStr line >> hFlush stdout

printStrLn :: Text.Text -> IO ()
printStrLn line = TextIO.putStrLn line >> hFlush stdout
