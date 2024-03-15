{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Actions.Server
( runProject
) where

import Mewlix.Project.Maker (ProjectMaker)
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Project.Data.Types (ProjectData(..), Port(..))
import Mewlix.Project.Actions.Build (buildProject)
import Mewlix.Project.Log (projectLog)
import Web.Scotty (scotty, middleware, get, file, addHeader)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless, void)
import System.Directory (doesDirectoryExist)
import Mewlix.Utils.Show (showT)
import System.FilePath ((</>))
import Web.Browser (openBrowser)

runServer :: (MonadIO m) => Port -> m ()
runServer port = liftIO . scotty (getPort port) $ do
    let policy = staticPolicy (addBase outputFolder)
    middleware policy
    get "/" $ do
        addHeader "Cache-Control" "max-age=0"
        file (outputFolder </> "index.html")

runProject :: ProjectData -> ProjectMaker ()
runProject projectData = do
    exists <- liftIO (doesDirectoryExist outputFolder)
    unless exists $
        buildProject projectData
    let port = projectPort projectData

    projectLog projectData $ mconcat
        [ "Running project "
        , (showT . projectName) projectData
        , " in port "
        , (showT . getPort) port
        ]

    -- Open browser:
    let address = concat ["http://127.0.0.1:", (show . getPort) port, "/"]
    (liftIO . void . openBrowser) address

    -- Run server:
    runServer port
