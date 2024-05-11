module Mewlix.Project.Actions.Run
( runProject
) where

import Mewlix.Project.Maker (ProjectMaker, liftIO)
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Project.Data.Types (ProjectData(..), ProjectMode(..))
import Mewlix.Project.Actions.Build (buildProject)
import Mewlix.Project.Actions.Server (serve)
import Mewlix.Project.Actions.Node (node)
import Control.Monad (unless)
import System.Directory (doesDirectoryExist)

runProject :: ProjectData -> ProjectMaker ()
runProject projectData = do
    exists <- liftIO (doesDirectoryExist outputFolder)
    unless exists $
        buildProject projectData

    case projectMode projectData of
        Console -> serve projectData
        Graphic -> serve projectData
        Library -> node projectData
