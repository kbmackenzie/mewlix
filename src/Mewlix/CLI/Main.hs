module Mewlix.CLI.Main
( run
) where

import Mewlix.CLI.Options (runOptions)
import Control.Monad (void)

run :: IO ()
run = void runOptions
