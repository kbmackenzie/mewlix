module Mewlix
( run
, make
, Action(..)
) where

import Mewlix.CLI.Main (run)
import Mewlix.Packager.Make (Action(..), make')

make :: Action -> IO ()
make = make'
