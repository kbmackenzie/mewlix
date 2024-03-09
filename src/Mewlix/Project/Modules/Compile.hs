{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Modules.Compile
( compileModules
) where

import Mewlix.Compiler (TranspilerContext(..))
import Mewlix.Project.Maker (ProjectMaker)
import Mewlix.Project.Data.Types
    ( ProjectData(..)
    , ProjectMode(..)
    , ProjectFlag(..)
    )
import Mewlix.Project.Modules.ModuleWriter (writeModule)
import Mewlix.Project.Modules.FileSearch (processSources, validateSources)
import Mewlix.Project.Log (projectLog)
import Mewlix.Utils.Show (showT)
import Mewlix.Abstract.Key (Key(..))
import Data.HashMap.Strict (mapKeys)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap

createContext :: ProjectData -> TranspilerContext
createContext projectData = do
    -- Implicitly adds the standard yarn ball + the mode yarn balls to the
    -- specialImports map:
    let addStd = HashMap.insert (Key "std") "Mewlix.Base"
    let addModeImports = case projectMode projectData of
            Console -> HashMap.insert (Key "std.console" ) "Mewlix.Console"
            Graphic -> HashMap.insert (Key "std.graphics") "Mewlix.Graphic"
            Library -> id
    let patchImports = addModeImports . addStd . mapKeys Key
    let flags = projectFlags projectData

    TranspilerContext
        { specialImports  = (patchImports . projectSpecialImports) projectData
        , transpilerNoStd = Set.member NoStd flags                              }

compileModules :: ProjectData -> ProjectMaker [FilePath]
compileModules projectData = do
    sources <- processSources (projectSourceFiles projectData)
    validateSources sources

    projectLog projectData $ mconcat
        ["Compiling ", (showT . length) sources, " yarn balls" ]

    let context = createContext projectData
    let compile :: FilePath -> ProjectMaker FilePath
        compile source = do
            projectLog projectData ("Compiling yarn ball " <> showT source)
            writeModule context source

    mapM compile sources
