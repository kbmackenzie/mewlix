{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Server
( runServer
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Environment (buildFolder)
import Mewlix.Packager.Config (ProjectConfig(..), Port(..), ProjectFlag(..))
import Mewlix.Packager.Log (projectLog)
import Mewlix.Utils.Server (serve)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, unless)
import Mewlix.Utils.Show (showT)
import Web.Browser (openBrowser)
import qualified Data.Set as Set

runServer :: ProjectConfig -> Packager ()
runServer config = do
    let port = projectPort config

    projectLog config $ mconcat
        [ "Running project "
        , (showT . projectName) config
        , " in port "
        , (showT . getPort) port ]

    -- Open browser:
    let noBrowser = Set.member NoBrowser (projectFlags config)
    unless noBrowser . void . liftIO $ do
        let address = concat ["http://localhost:", (show . getPort) port, "/"]
        openBrowser address

    -- Run server:
    projectLog config "Press (Ctrl + C) to exit."
    serve buildFolder (getPort port)
