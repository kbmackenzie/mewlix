module Mewlix.Packager.Actions.Clean
( cleanProject
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Log (logMessage)
import Mewlix.Packager.Config (ProjectConfig)
import Mewlix.Packager.Build.Clean (clean)

cleanProject :: ProjectConfig -> Packager ()
cleanProject config = do
    logMessage config "Cleaning output folder..."
    clean
