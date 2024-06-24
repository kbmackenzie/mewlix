{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Packager.Modules.Compile
( compileModules
) where

import Mewlix.Packager.Maker
    ( PackageMaker
    , throwError
    , liftIO
    )
import Mewlix.Compiler
    ( TranspilerContext(..)
    , CompilerOutput
    , compileJS
    )
import Mewlix.Packager.Data.Types (ProjectData(..), ProjectFlag(..))
import Mewlix.Packager.Modules.StandardLibrary (addLibraries)
import Mewlix.Packager.Modules.FileSearch (processSources, validateSources)
import Mewlix.Packager.Log (projectLog)
import Mewlix.Utils.Show (showT)
import Mewlix.Utils.FileIO (readText)
import Mewlix.Abstract.Key (Key(..))
import Data.HashMap.Strict (mapKeys)
import qualified Data.Set as Set
import System.IO (Handle)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO

-- Creates the transpiler context.
createContext :: ProjectData -> PackageMaker TranspilerContext
createContext projectData = do
    let projectLibs = addLibraries (projectMode projectData)
    let createImportMap = projectLibs . mapKeys Key
    let flags = projectFlags projectData

    return TranspilerContext
        { imports = createImportMap mempty
        , noStd   = Set.member NoStd flags
        , pretty  = Set.member Pretty flags }

-- Compile all yarn balls, writing the compilation output to a handle.
compileModules :: ProjectData -> Handle -> PackageMaker ()
compileModules projectData handle = do
    sources <- processSources (projectSourceFiles projectData)
    validateSources sources

    let sourceCount = length sources
    projectLog projectData $ mconcat
        [ "Compiling "
        , showT sourceCount
        , if sourceCount > 1 then " yarn balls!" else " yarn ball!" ]
    context  <- createContext projectData

    let write :: Text -> PackageMaker ()
        write = liftIO . TextIO.hPutStrLn handle

    let compile :: FilePath -> PackageMaker ()
        compile path = do
            projectLog projectData ("Compiling yarn ball " <> showT path)
            yarnball <- runCompiler context path
            write yarnball

    write "export default function(mewlix) {\n"
    mapM_ compile sources
    write "}\n"

-- Runs compiler on text content read from a file.
runCompiler :: TranspilerContext -> FilePath -> PackageMaker CompilerOutput
runCompiler context path = readText path >>= \case
    (Left err)       -> throwError . concat $ [ "Couldn't read file ", show path, ": ", err ]
    (Right contents) -> case compileJS context path contents of
        (Left err)       -> throwError . concat $ [ "Mewlix syntax error in file ", show path, ":\n", err ]
        (Right yarnball) -> return yarnball
