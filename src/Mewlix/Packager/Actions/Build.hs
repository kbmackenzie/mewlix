{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Build
( buildProject
) where

import Mewlix.Packager.Type (Packager, liftIO)
import Mewlix.Packager.Config (ProjectConfig(..))
import Mewlix.Packager.Modules (bundleModules)
import Mewlix.Packager.Templates (generateTemplate, writeReadMe)
import Mewlix.Packager.Assets (copyAssets)
import Mewlix.Packager.Log (projectLog)
import Mewlix.Packager.Folder (mewlixFolder, coreFolder)
import Mewlix.Utils.IO (createDirectory)
import System.FilePath ((</>))
import Data.Aeson (object, (.=), encode, Value)
import qualified Data.ByteString.Lazy as LB

buildProject :: ProjectConfig -> Packager ()
buildProject config = do
    projectLog config $ mconcat
        ["Building project '", projectName config, "'"]

    -- Template:
    projectLog config "Creating template..."
    createDirectory True mewlixFolder
    generateTemplate config

    -- Modules:
    bundleModules config
    copyAssets config
    metaData config
    writeReadMe config

metaData :: ProjectConfig -> Packager ()
metaData config = do
    let targetPath = coreFolder </> "meta.json"
    let json = object
            [ "title"       .= projectName config
            , "entrypoint"  .= projectEntrypoint config
            , "description" .= projectDescription config ]

    let write :: Value -> Packager ()
        write = liftIO . LB.writeFile targetPath . (<> "\n") . encode

    write json
