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
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap

createContext :: ProjectData -> TranspilerContext
createContext projectData = do
    let addModeImports = case projectMode projectData of
            Console -> HashMap.insert (Key "std.console" ) "Mewlix.MewlixConsole"
            Graphic -> HashMap.insert (Key "std.graphics") "Mewlix.MewlixGraphics"
            Library -> id
    let flags = projectFlags projectData

    TranspilerContext
        { specialImports  = (addModeImports . mapKeys Key . projectSpecialImports) projectData
        , transpilerNoStd = Set.member NoStd flags                                              }

compileModules :: ProjectData -> ProjectMaker [FilePath]
compileModules projectData = do
    sources <- processSources (projectSourceFiles projectData)
    validateSources sources

    projectLog projectData $ mconcat
        ["Compiling ", (showT . length) sources, " modules" ]

    let context = createContext projectData
    let compile :: FilePath -> ProjectMaker FilePath
        compile source = do
            projectLog projectData $ mconcat
                ["Compiling module \"", Text.pack source, "\""]
            writeModule context source

    mapM compile sources
