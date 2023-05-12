{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.API.Console
( meow
) where

import Meowscript.Core.AST
import Meowscript.Core.Pretty
import Meowscript.Core.RunEvaluator
import Meowscript.Core.Exceptions
import qualified Data.Text as Text
import Meowscript.Utils.IO
import System.Console.ANSI.Types
import qualified System.Console.ANSI as Console

meow :: FilePath -> IO ()
meow path = runMeow path >>= \case
    (Left exception) -> printError exception
    (Right output) -> printStrLn output
