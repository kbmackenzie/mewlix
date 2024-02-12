module Mewlix.Project.Actions.Build
( buildProject
) where

import Mewlix.Project.Maker (ProjectMaker, liftIO)
import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Project.Modules.Compile (compileModules)
import Mewlix.Project.Templates.Create (createFromTemplate)
import qualified Data.List as List
import Mewlix.Utils.FileIO (writeFileS)

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
    liftIO $ writeFileS targetPath scripts
