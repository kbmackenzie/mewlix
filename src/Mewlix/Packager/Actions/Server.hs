{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Server
( serve
) where

import Mewlix.Packager.Maker (PackageMaker)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Packager.Data.Types (ProjectData(..), Port(..))
import Mewlix.Packager.Log (projectLog)
import Web.Scotty (scottyApp, middleware, get, file, addHeader)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (void)
import Mewlix.Utils.Show (showT)
import System.FilePath ((</>))
import Web.Browser (openBrowser)

runServer :: (MonadIO m) => Port -> m ()
runServer port = run (getPort port) $ do
    let policy = staticPolicy (addBase outputFolder)
    middleware policy
    get "/" $ do
        addHeader "Cache-Control" "no-cache"
        file (outputFolder </> "index.html")
    where run p s = liftIO (Warp.run p =<< scottyApp s)

serve :: ProjectData -> PackageMaker ()
serve projectData = do
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
    runServer port
