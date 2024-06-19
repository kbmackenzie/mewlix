{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Utils.Server
( serve
) where

import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.Wai (Application, pathInfo, requestMethod, responseFile, responseLBS)
import Network.HTTP.Types (methodGet, status200, status404, Header)
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.FilePath ((</>))

html :: Header
html = ("Content-Type", "text/html; charset=utf-8")

noCache :: Header
noCache = ("Cache-Control", "no-cache")

-- Assume 'staticPolicy (addBase ...)' has already been set as middleware.
-- That is necessary when serving the entrypoint file.
serveRoot :: FilePath -> Application
serveRoot base req respond
    | requestMethod req == methodGet && (null . pathInfo) req = do
        let headers = [html, noCache]
        let index = base </> "index.html"
        respond $ responseFile status200 headers index Nothing
    -- Do not handle any other form of request.
    | otherwise = do
        let message = fromString "<h1>404: not found 🐱</h1>"
        respond $ responseLBS status404 [html] message

serve :: (MonadIO m) => FilePath -> Port -> m ()
serve base port = do
    let middleware = staticPolicy (addBase base)
    let app = middleware (serveRoot base)
    liftIO (Warp.run port app)
