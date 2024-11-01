module Mewlix.Packager.Actions.Build
( buildProject
) where

import Mewlix.Packager.Config (ProjectConfig)
import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Build (build)

buildProject :: ProjectConfig -> Packager ()
buildProject = build
