{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Modules.Compile
( compileModules
) where

import Mewlix.Packager.Type (Packager, throwError)
import Mewlix.Compiler
    ( TranspilerContext(..)
    , CompilerOutput
    , compileJS
    )
import Mewlix.Packager.Config (ProjectConfig(..), ProjectFlag(..))
import Mewlix.Packager.Modules.StandardLibrary (addLibraries)
import System.FilePattern.Directory (getDirectoryFiles)
import Mewlix.Packager.Log (projectLog)
import Mewlix.Utils.Show (showT)
import Mewlix.Utils.IO (safelyRun, readFileText)
import qualified Data.Set as Set
import System.IO (Handle)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO

-- Creates the transpiler context.
createContext :: ProjectConfig -> Packager TranspilerContext
createContext config = do
    let projectLibs = addLibraries (projectMode config) mempty
    let flags = projectFlags config

    return TranspilerContext
        { library = projectLibs
        , noStd   = Set.member NoStd flags
        , pretty  = Set.member Pretty flags }

findSources :: ProjectConfig -> Packager [FilePath]
findSources config = do
    let patterns = projectSourceFiles config
    safelyRun (getDirectoryFiles "." patterns) "couldn't get source files"

-- Compile all yarn balls, writing the compilation output to a handle.
compileModules :: ProjectConfig -> Handle -> Packager ()
compileModules config handle = do
    sources <- findSources config

    let sourceCount = length sources
    projectLog config $ mconcat
        [ "Compiling "
        , showT sourceCount
        , if sourceCount > 1 then " yarn balls!" else " yarn ball!" ]
    context <- createContext config

    let write :: Text -> Packager ()
        write text = safelyRun (TextIO.hPutStrLn handle text) "couldn't write to file handle"

    let compile :: FilePath -> Packager ()
        compile path = do
            projectLog config ("Compiling yarn ball " <> showT path)
            yarnball <- runCompiler context path
            write yarnball

    write "export default function(mewlix) {\n"
    mapM_ compile sources
    write "}\n"

-- Runs compiler on text content read from a file.
runCompiler :: TranspilerContext -> FilePath -> Packager CompilerOutput
runCompiler context path = do
    contents <- readFileText path
    case compileJS context path contents of
        (Left err)       -> throwError . concat $ [ "Mewlix syntax error in file ", show path, ":\n", err ]
        (Right yarnball) -> return yarnball
