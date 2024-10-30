{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Server
( runServer
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Folder (buildFolder)
import Mewlix.Packager.Config (ProjectConfig(..), Port(..))
import Mewlix.Packager.Log (projectLog)
import Mewlix.Utils.Server (serve)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Mewlix.Utils.Show (showT)
import Web.Browser (openBrowser)

runServer :: ProjectConfig -> Packager ()
runServer config = do
    let port = projectPort config

    projectLog config $ mconcat
        [ "Running project "
        , (showT . projectName) config
        , " in port "
        , (showT . getPort) port ]

    -- Open browser:
    let address = concat ["http://localhost:", (show . getPort) port, "/"]
    (liftIO . void . openBrowser) address

    -- Run server:
    projectLog config "Press (Ctrl + C) to exit."
    serve buildFolder (getPort port)
