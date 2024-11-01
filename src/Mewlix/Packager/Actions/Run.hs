module Mewlix.Packager.Actions.Run
( runProject
) where

import Mewlix.Packager.Type (Packager, liftIO)
import Mewlix.Packager.Environment (buildFolder)
import Mewlix.Packager.Config (ProjectConfig(..), ProjectMode(..), ProjectFlag(..))
import Mewlix.Packager.Actions.Build (buildProject)
import Mewlix.Packager.Actions.Server (runServer)
import Mewlix.Packager.Actions.Node (runNode)
import Control.Monad (when)
import System.Directory (doesDirectoryExist)
import qualified Data.Set as Set

runProject :: ProjectConfig -> Packager ()
runProject config = do
    let rebuild = Set.member Rebuild (projectFlags config)
    exists <- liftIO (doesDirectoryExist buildFolder)

    when (not exists || rebuild) $
        buildProject config

    case projectMode config of
        Console -> runServer config
        Graphic -> runServer config
        Node    -> runNode config
