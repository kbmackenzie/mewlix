{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Actions.Server
( runProject
) where

import Mewlix.Project.Maker (ProjectMaker)
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Project.Data.Types (ProjectData(..), defaultPort)
import Mewlix.Project.Actions.Build (buildProject)
import Mewlix.Project.Log (projectLog)
import Web.Scotty (scotty, middleware, get, file)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import System.Directory (doesDirectoryExist)
import Mewlix.Utils.Show (showT)

runServer :: (MonadIO m) => Port -> m ()
runServer port = liftIO . scotty port $ do
    let policy = staticPolicy (addBase outputFolder)
    middleware policy
    get "/" $ file "index.html"

runProject :: ProjectData -> ProjectMaker ()
runProject projectData = do
    exists <- liftIO (doesDirectoryExist outputFolder)
    unless exists $
        buildProject projectData

    let port = defaultPort
    projectLog projectData $ mconcat
        ["Running project ", projectName projectData, " in port ", showT port]
    runServer port
