module Mewlix
( run
, make
, Action(..)
) where

import Mewlix.CLI.Main (run)
import Mewlix.Project.Make (Action(..), make', Language(..))

make :: Action -> IO ()
make = make' JavaScript
