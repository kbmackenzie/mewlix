module Mewlix.Packager.Actions.Run
( runProject
) where

import Mewlix.Packager.Maker (PackageMaker, liftIO)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Packager.Data.Types (ProjectData(..), ProjectMode(..))
import Mewlix.Packager.Actions.Build (buildProject)
import Mewlix.Packager.Actions.Server (serve)
import Mewlix.Packager.Actions.Node (node)
import Control.Monad (unless)
import System.Directory (doesDirectoryExist)

runProject :: ProjectData -> PackageMaker ()
runProject projectData = do
    exists <- liftIO (doesDirectoryExist outputFolder)
    unless exists $
        buildProject projectData

    case projectMode projectData of
        Console -> serve projectData
        Graphic -> serve projectData
        Library -> node projectData
