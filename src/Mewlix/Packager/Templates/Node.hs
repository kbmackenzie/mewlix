{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Templates.Node
( writePackageData
) where

import Mewlix.Packager.Config (ProjectData(..))
import Mewlix.Packager.Folder (outputFolder)
import Data.Aeson (Value, object, (.=), encode)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as ByteString

packageData :: ProjectData -> Value
packageData projectData = object
    [ "name"        .= projectName projectData
    , "version"     .= ("1.0.0" :: String)
    , "description" .= projectDescription projectData
    , "type"        .= ("module" :: String)           ]

writePackageData :: (MonadIO m) => ProjectData -> m ()
writePackageData projectData = do
    let package = packageData projectData
    let path = outputFolder </> "package.json"
    liftIO $ do
        ByteString.writeFile path (encode package)
