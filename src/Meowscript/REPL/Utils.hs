module Meowscript.REPL.Utils
( printStr
, printStrLn
) where

import Meowscript.Core.AST
import Meowscript.Core.Pretty
import Meowscript.Core.RunEvaluator 
import Meowscript.Core.Keys
import Meowscript.Core.Blocks (evaluate)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
import Control.Monad (void, join)
import Control.Monad.Reader (ask)
import Data.IORef
import Data.Functor((<&>))
import System.IO (hFlush, stdout)

-- Flush stdout after printing so that it happens before getLine.
printStr :: Text.Text -> IO ()
printStr line = TextIO.putStr line >> hFlush stdout

printStrLn :: Text.Text -> IO ()
printStrLn line = TextIO.putStr line >> hFlush stdout
