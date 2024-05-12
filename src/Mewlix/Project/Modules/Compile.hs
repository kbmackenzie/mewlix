{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Modules.Compile
( compileModules
) where

import Mewlix.Project.Maker (ProjectMaker, ProjectContext(..), asks)
import Mewlix.Compiler (TranspilerContext(..))
import Mewlix.Project.Data.Types (ProjectData(..), ProjectFlag(..))
import Mewlix.Project.Modules.StandardLibrary (addLibraries)
import Mewlix.Project.Modules.ModuleWriter (writeModule)
import Mewlix.Project.Modules.FileSearch (processSources, validateSources)
import Mewlix.Project.Log (projectLog)
import Mewlix.Utils.Show (showT)
import Mewlix.Abstract.Key (Key(..))
import Data.HashMap.Strict (mapKeys)
import qualified Data.Set as Set

createContext :: ProjectData -> ProjectMaker TranspilerContext
createContext projectData = do
    language <- asks projectLanguage
    let projectLibs = addLibraries language (projectMode projectData)
    let createImportMap = projectLibs . mapKeys Key . projectSpecialImports
    let flags = projectFlags projectData

    return TranspilerContext
        { imports = createImportMap projectData
        , noStd   = Set.member NoStd flags
        , pretty  = False {- todo! -}           }

compileModules :: ProjectData -> ProjectMaker ()
compileModules projectData = do
    sources <- processSources (projectSourceFiles projectData)
    validateSources sources

    projectLog projectData $ mconcat
        ["Compiling ", (showT . length) sources, " yarn balls!" ]

    context <- createContext projectData

    let compile :: FilePath -> ProjectMaker ()
        compile source = do
            projectLog projectData ("Compiling yarn ball " <> showT source)
            writeModule context source

    mapM_ compile sources
