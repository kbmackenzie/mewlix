{-# LANGUAGE LambdaCase #-}

module Mewlix.CLI.Main 
( run
) where

import Mewlix.Project.Make (Action(..), Language(..), make, makeSingle)
import qualified Mewlix.CLI.Options as Options

run :: IO ()
run = Options.runOptions >>= \case
    (Options.Build {})  -> undefined
    Options.Clean       -> undefined
