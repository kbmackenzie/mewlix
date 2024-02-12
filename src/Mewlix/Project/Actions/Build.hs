module Mewlix.Project.Actions.Build
( project
, buildProject
) where

import Mewlix.Project.ProjectMaker (ProjectMaker, liftIO)
import Mewlix.Project.ReadProject (readProject)
import Mewlix.Project.ProjectData (ProjectData(..))
import Mewlix.Project.Modules.Compile (compileModules)
import Mewlix.Project.Templates.Template (createFromTemplate)
import qualified Data.List as List
import qualified Mewlix.Utils.FileIO as FileIO

buildProject :: ProjectData -> ProjectMaker ()
buildProject projectData = do
    -- Template:
    createFromTemplate (projectMode projectData)

    -- Modules:
    compiledModules <- compileModules projectData
    scriptList compiledModules

scriptList :: [FilePath] -> ProjectMaker ()
scriptList paths = do
    let targetPath = "output/mewlix/core/script-list"
    let scripts = List.intercalate "\n" paths
    liftIO $ FileIO.writeFileS targetPath scripts

project :: ProjectMaker ()
project = readProject >>= buildProject
