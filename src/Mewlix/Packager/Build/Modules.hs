{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Build.Modules
( compileModules
) where

import Mewlix.Packager.Type
    ( Packager
    , packager
    , liftIO
    , liftEither
    , throwError
    )
import Mewlix.Compiler
    ( TranspilerContext(..)
    , compileJS
    )
import Mewlix.Packager.Config (ProjectConfig(..), ProjectFlag(..))
import Mewlix.Packager.Library (getLibrary)
import Mewlix.Packager.Log (projectLog)
import Mewlix.Utils.Show (showT)
import Mewlix.Utils.IO (safelyRun, readFileText, createDirectory)
import Mewlix.Packager.Environment (moduleFolder)
import Control.Monad ((>=>))
import System.FilePath ((</>))
import System.FilePattern.Directory (getDirectoryFiles)
import System.IO (IOMode(..), withFile)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Set as Set

-- Creates the transpiler context.
createContext :: ProjectConfig -> Packager TranspilerContext
createContext config = do
    let projectLibs = getLibrary (projectMode config)
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
compileModules :: ProjectConfig -> Packager ()
compileModules config = do
    let target = moduleFolder </> "yarnball.js"
    createDirectory False moduleFolder
    sources <- findSources config

    let sourceCount = length sources
    projectLog config $ mconcat
        [ "Compiling "
        , showT sourceCount
        , if sourceCount > 1 then " yarn balls!" else " yarn ball!" ]
    context <- createContext config
           
    -- Runs compiler on text content read from a file.
    let compile :: FilePath -> Packager Text
        compile source = do
            let syntaxError :: String -> String
                syntaxError err = concat [ "Syntax error in file ", show source, ":\n", err ]
            contents <- readFileText source
            either (throwError . syntaxError) return (compileJS context source contents)

    (liftIO >=> liftEither) . withFile target WriteMode $ \handle -> packager $ do
        let write :: Text -> Packager ()
            write text = do
                let errorMessage = "couldn't write to file handle"
                safelyRun (TextIO.hPutStrLn handle text) errorMessage

        let compileSource :: FilePath -> Packager ()
            compileSource source = do
                projectLog config ("Compiling yarn ball " <> showT source)
                compile source >>= write

        write metaComment
        write "\n\n"
        write "export default function(mewlix) {\n"
        mapM_ compileSource sources
        write "}\n"

metaComment :: Text
metaComment = Text.intercalate "\n"
    [ "/*     Auto-generated by the Mewlix compiler"
    , " * > https://www.github.com/kbmackenzie/mewlix < */" ]
