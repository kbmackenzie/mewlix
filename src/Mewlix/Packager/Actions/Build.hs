{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Build
( buildProject
) where

import Mewlix.Packager.Type (Packager, liftIO)
import Mewlix.Packager.Config.Types (ProjectData(..))
import Mewlix.Packager.Modules.Bundle (bundleModules)
import Mewlix.Packager.Folder (coreFolder)
import Mewlix.Packager.Templates.Create (createFromTemplate)
import Mewlix.Packager.Templates.ReadMe (createReadme)
import Mewlix.Packager.Assets.Copy (copyAssets)
import Mewlix.Packager.Log (projectLog)
import System.FilePath ((</>))
import Data.Aeson (object, (.=), encode, Value)
import qualified Data.ByteString.Lazy as LB

buildProject :: ProjectData -> Packager ()
buildProject projectData = do
    projectLog projectData $ mconcat
        ["Building project '", projectName projectData, "'"]

    -- Template:
    projectLog projectData "Creating template..."
    createFromTemplate projectData

    -- Modules:
    bundleModules projectData
    copyAssets projectData
    metaData projectData
    createReadme projectData

metaData :: ProjectData -> Packager ()
metaData projectData = do
    let targetPath = coreFolder </> "meta.json"
    let json = object
            [ "title"       .= projectName projectData
            , "entrypoint"  .= projectEntrypoint projectData
            , "description" .= projectDescription projectData ]

    let write :: Value -> Packager ()
        write = liftIO . LB.writeFile targetPath . (<> "\n") . encode

    write json
