{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Actions.Build
( buildProject
) where

import Mewlix.Project.Maker (ProjectMaker)
import Mewlix.Project.Folder (coreFolder)
import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Project.Modules.Compile (compileModules)
import Mewlix.Project.Templates.Create (createFromTemplate)
import System.FilePath ((</>))
import Mewlix.Utils.FileIO (writeFileBL)
import Data.Aeson (object, (.=), encode)

buildProject :: ProjectData -> ProjectMaker ()
buildProject projectData = do
    -- Template:
    createFromTemplate (projectMode projectData)

    -- Modules:
    compiledModules <- compileModules projectData
    scriptList projectData compiledModules

scriptList :: ProjectData -> [FilePath] -> ProjectMaker ()
scriptList projectData paths = do
    targetPath <- (</> "script-list") <$> coreFolder
    let entrypoint = projectEntrypoint projectData
    let script = object
            [ "entrypoint" .= entrypoint
            , "scripts"    .= paths         ]
    writeFileBL targetPath (encode script)
