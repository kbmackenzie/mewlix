{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Server
( runServer
) where

import Mewlix.Packager.Maker (PackageMaker)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Packager.Data.Types (ProjectData(..), Port(..))
import Mewlix.Packager.Log (projectLog)
import Mewlix.Utils.Server (serve)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Mewlix.Utils.Show (showT)
import Web.Browser (openBrowser)

runServer :: ProjectData -> PackageMaker ()
runServer projectData = do
    let port = projectPort projectData

    projectLog projectData $ mconcat
        [ "Running project "
        , (showT . projectName) projectData
        , " in port "
        , (showT . getPort) port ]

    -- Open browser:
    let address = concat ["http://localhost:", (show . getPort) port, "/"]
    (liftIO . void . openBrowser) address

    -- Run server:
    projectLog projectData "Press (Ctrl + C) to exit."
    serve outputFolder (getPort port)
