{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Actions.Build
( buildProject
) where

import Mewlix.Project.Maker (ProjectMaker)
import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Project.Modules.Compile (compileModules)
import Mewlix.Project.Folder (coreFolder)
import Mewlix.Project.Templates.Create (createFromTemplate)
import Mewlix.Project.Templates.ReadMe (createReadme)
import Mewlix.Project.Assets.Copy (copyAssets)
import Mewlix.Project.Log (projectLog)
import System.FilePath ((</>))
import Mewlix.Utils.FileIO (writeBytesLazy)
import Data.Aeson (object, (.=), encode)

buildProject :: ProjectData -> ProjectMaker ()
buildProject projectData = do
    projectLog projectData $ mconcat
        ["Building project '", projectName projectData, "'"]

    -- Template:
    projectLog projectData "Creating template..."
    createFromTemplate (projectMode projectData)

    -- Modules:
    compileModules projectData
    copyAssets projectData
    scriptList projectData
    createReadme projectData

scriptList :: ProjectData -> ProjectMaker ()
scriptList projectData = do
    let targetPath = coreFolder </> "script-list.json"
    let title = projectName projectData
    let entrypoint = projectEntrypoint projectData

    let json = object
            [ "title"      .= title
            , "entrypoint" .= entrypoint ]

    let contents = mappend (encode json) "\n"
    writeBytesLazy targetPath contents
