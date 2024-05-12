module Mewlix
( run
, make
, Action(..)
) where

import Mewlix.CLI.Main (run)
import Mewlix.Packager.Make (Action(..), make', Language(..))

make :: Action -> IO ()
make = make' JavaScript
