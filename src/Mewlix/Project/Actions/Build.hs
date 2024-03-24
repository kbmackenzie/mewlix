{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Actions.Build
( buildProject
) where

import Mewlix.Project.Maker (ProjectMaker)
import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Project.Folder (coreFolder, outputFolder)
import Mewlix.Project.Modules.Compile (compileModules)
import Mewlix.Project.Templates.Create (createFromTemplate)
import Mewlix.Project.Templates.ReadMe (createReadme)
import Mewlix.Project.Assets.Copy (copyAssets)
import Mewlix.Project.Log (projectLog)
import System.FilePath ((</>), makeRelative)
import Mewlix.Utils.FileIO (writeFileBL)
import Data.Aeson (object, (.=), encode)

buildProject :: ProjectData -> ProjectMaker ()
buildProject projectData = do
    projectLog projectData $ mconcat
        ["Building project '", projectName projectData, "'"]

    -- Template:
    projectLog projectData "Creating template..."
    createFromTemplate (projectMode projectData)

    -- Modules:
    compiledModules <- compileModules projectData
    copyAssets projectData
    createReadme projectData
    scriptList projectData compiledModules

scriptList :: ProjectData -> [FilePath] -> ProjectMaker ()
scriptList projectData paths = do
    let targetPath = coreFolder </> "script-list.json"
    let entrypoint = projectEntrypoint projectData
    let relativePaths = map (makeRelative outputFolder) paths

    let json = object
            [ "entrypoint" .= entrypoint
            , "scripts"    .= relativePaths ]

    let contents = mappend (encode json) "\n"
    writeFileBL targetPath contents
