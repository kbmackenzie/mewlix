{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Modules.Compile
( compileModules
) where

import Mewlix.Compiler (TranspilerContext(..))
import Mewlix.Project.Maker (ProjectMaker)
import Mewlix.Project.Data.Types (ProjectData(..), ProjectMode(..))
import Mewlix.Project.Modules.ModuleWriter (writeModules)
import Mewlix.Project.Modules.FileSearch (processSources, validateSources)
import Mewlix.Abstract.Key (Key(..))
import Data.HashMap.Strict (mapKeys)
import qualified Data.HashMap.Strict as HashMap

createContext :: ProjectData -> TranspilerContext
createContext projectData = do
    let addModeImports = case projectMode projectData of
            Console -> HashMap.insert (Key "std.console" ) "Mewlix.MewlixConsole"
            Graphic -> HashMap.insert (Key "std.graphics") "Mewlix.MewlixGraphics"
            Library -> id

    TranspilerContext
        { specialImports  = (addModeImports . mapKeys Key . projectSpecialImports) projectData
        , transpilerFlags = projectFlags projectData                                            }

compileModules :: ProjectData -> ProjectMaker [FilePath]
compileModules projectData = do
    sources <- processSources (projectSourceFiles projectData)
    validateSources sources

    let context = createContext projectData
    writeModules context sources
