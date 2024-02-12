module Mewlix.Project.Modules.Compile
( compileModules
) where

import Mewlix.Compiler (TranspilerContext(..))
import Mewlix.Project.ProjectMaker (ProjectMaker)
import Mewlix.Project.ProjectData (ProjectData(..))
import Mewlix.Project.Modules.ModuleWriter (writeModules)
import Mewlix.Project.Modules.FileSearch (processSources, validateSources)
import Mewlix.Abstract.Key (Key(..))
import Data.HashMap.Strict (mapKeys)

createContext :: ProjectData -> TranspilerContext
createContext projectData = TranspilerContext 
    { specialImports  = (mapKeys Key . projectSpecialImports) projectData
    , transpilerFlags = projectFlags projectData                          }

compileModules :: ProjectData -> ProjectMaker [FilePath]
compileModules projectData = do
    sources <- processSources (projectSourceFiles projectData)
    validateSources sources

    let context = createContext projectData
    writeModules context sources
