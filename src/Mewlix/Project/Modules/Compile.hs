{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Modules.Compile
( compileModules
) where

import Mewlix.Project.Maker
    ( ProjectMaker
    , ProjectContext(..)
    , asks
    , throwError
    , liftIO
    )
import Mewlix.Compiler
    ( TranspilerContext(..)
    , CompilerFunc
    , CompilerOutput
    )
import Mewlix.Project.Data.Types (ProjectData(..), ProjectFlag(..))
import Mewlix.Project.Modules.StandardLibrary (addLibraries)
import Mewlix.Project.Modules.FileSearch (processSources, validateSources)
import Mewlix.Project.Log (projectLog)
import Mewlix.Utils.Show (showT)
import Mewlix.Utils.FileIO (readText)
import Mewlix.Abstract.Key (Key(..))
import Data.HashMap.Strict (mapKeys)
import qualified Data.Set as Set
import System.IO (Handle)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO

-- Creates the transpiler context.
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

-- Compile all yarn balls, writing the compilation output to a handle.
compileModules :: ProjectData -> Handle -> ProjectMaker ()
compileModules projectData handle = do
    sources <- processSources (projectSourceFiles projectData)
    validateSources sources

    let sourceCount = length sources
    projectLog projectData $ mconcat
        [ "Compiling "
        , showT sourceCount
        , if sourceCount > 1 then " yarn balls!" else " yarn ball!" ]

    context  <- createContext projectData
    compiler <- asks projectCompiler

    let write :: Text -> ProjectMaker ()
        write = liftIO . TextIO.hPutStrLn handle

    let compile :: FilePath -> ProjectMaker ()
        compile path = do
            projectLog projectData ("Compiling yarn ball " <> showT path)
            yarnball <- runCompiler compiler context path
            write yarnball

    mapM_ compile sources

-- Runs a compiler function on text content read from a file.
runCompiler :: CompilerFunc -> TranspilerContext -> FilePath -> ProjectMaker CompilerOutput
runCompiler compile context path = readText path >>= \case
    (Left err)       -> throwError . concat $ [ "Couldn't read file ", show path, ": ", show err ]
    (Right contents) -> case compile context path contents of
        (Left err)       -> throwError . concat $ [ "Mewlix syntax error in file ", show path, ":\n", err ]
        (Right yarnball) -> return yarnball
