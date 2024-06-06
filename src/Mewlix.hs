module Mewlix
( make
, Action(..)
) where

import Mewlix.Packager.Make (Action(..), make')

make :: Action -> IO ()
make = make'
