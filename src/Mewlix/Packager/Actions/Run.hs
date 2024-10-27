module Mewlix.Packager.Actions.Run
( runProject
) where

import Mewlix.Packager.Type (Packager, liftIO)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Packager.Config.Types (ProjectData(..), ProjectMode(..), ProjectFlag(..))
import Mewlix.Packager.Actions.Build (buildProject)
import Mewlix.Packager.Actions.Server (runServer)
import Mewlix.Packager.Actions.Node (runNode)
import Control.Monad (when)
import System.Directory (doesDirectoryExist)
import qualified Data.Set as Set

runProject :: ProjectData -> Packager ()
runProject projectData = do
    let rebuild = Set.member Rebuild (projectFlags projectData)
    exists <- liftIO (doesDirectoryExist outputFolder)

    when (not exists || rebuild) $
        buildProject projectData

    case projectMode projectData of
        Console -> runServer projectData
        Graphic -> runServer projectData
        Node    -> runNode projectData
