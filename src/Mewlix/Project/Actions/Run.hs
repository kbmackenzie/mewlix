module Mewlix.Project.Actions.Run
( runProject
) where

import Mewlix.Project.Maker (ProjectMaker, liftIO)
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Project.Data.Types (ProjectData(..), ProjectMode(..))
import Mewlix.Project.Actions.Build (buildProject)
import Control.Monad (unless)
import System.Directory (doesDirectoryExist)

runProject :: ProjectData -> ProjectMaker ()
runProject projectData = do
    exists <- liftIO (doesDirectoryExist outputFolder)
    unless exists $
        buildProject projectData

    case projectMode projectData of
        Console -> undefined
        Graphic -> undefined
        Library -> undefined
