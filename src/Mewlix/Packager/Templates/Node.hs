{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Templates.Node
( writePackageData
) where

import Mewlix.Packager.Config (ProjectData(..))
import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Utils.IO (safelyRun)
import Data.Aeson (Value, object, (.=), encode)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as ByteString

packageData :: ProjectData -> Value
packageData projectData = object
    [ "name"        .= projectName projectData
    , "version"     .= ("1.0.0" :: String)
    , "description" .= projectDescription projectData
    , "type"        .= ("module" :: String)           ]

writePackageData :: ProjectData -> Packager ()
writePackageData projectData = do
    let package = packageData projectData
    let path = outputFolder </> "package.json"

    let action = ByteString.writeFile path (encode package)
    safelyRun action "couldn't write package data"
