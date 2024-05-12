{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Build
( buildProject
) where

import Mewlix.Packager.Maker (ProjectMaker, liftIO)
import Mewlix.Packager.Data.Types (ProjectData(..))
import Mewlix.Packager.Modules.Bundle (bundleModules)
import Mewlix.Packager.Folder (coreFolder)
import Mewlix.Packager.Templates.Create (createFromTemplate)
import Mewlix.Packager.Templates.ReadMe (createReadme)
import Mewlix.Packager.Assets.Copy (copyAssets)
import Mewlix.Packager.Log (projectLog)
import System.FilePath ((</>))
import Data.Aeson (object, (.=), encode, Value)
import qualified Data.ByteString.Lazy as LB

buildProject :: ProjectData -> ProjectMaker ()
buildProject projectData = do
    projectLog projectData $ mconcat
        ["Building project '", projectName projectData, "'"]

    -- Template:
    projectLog projectData "Creating template..."
    createFromTemplate (projectMode projectData)

    -- Modules:
    bundleModules projectData
    copyAssets projectData
    metaData projectData
    createReadme projectData

metaData :: ProjectData -> ProjectMaker ()
metaData projectData = do
    let targetPath = coreFolder </> "meta.json"
    let title = projectName projectData
    let entrypoint = projectEntrypoint projectData

    let json = object
            [ "title"      .= title
            , "entrypoint" .= entrypoint ]

    let write :: Value -> ProjectMaker ()
        write = liftIO . LB.writeFile targetPath . (<> "\n") . encode

    write json
