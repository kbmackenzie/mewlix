{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Modules.Bundle
( bundleModules
) where

import Mewlix.Packager.Maker
    ( PackageMaker
    , throwError
    , catchError
    , liftIO
    )
import Mewlix.Packager.Data.Types (ProjectData(..))
import Mewlix.Packager.Modules.Compile (compileModules)
import Mewlix.Packager.Folder (moduleFolder)
import System.IO (IOMode(..), openFile, hClose)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

metaComment :: Text
metaComment = Text.intercalate "\n"
    [ "/*     Auto-generated by the Mewlix compiler"
    , " * > https://www.github.com/kbmackenzie/mewlix < */" ]

-- Compile and bundle all modules into a single .js file.
-- It's simpler this way!
bundleModules :: ProjectData -> PackageMaker ()
bundleModules projectData = do
    let outputPath = moduleFolder </> "yarnballs.js"
    
    handle <- liftIO $ do
        createDirectoryIfMissing False moduleFolder
        openFile outputPath WriteMode

    let write :: Text -> PackageMaker ()
        write = liftIO . TextIO.hPutStr handle

    write metaComment 
    write "\n\n"

    let close :: PackageMaker ()
        close = liftIO (hClose handle)

    let compile :: PackageMaker () 
        compile = compileModules projectData handle >> close

    let closeOnError :: String -> PackageMaker ()
        closeOnError e = close >> throwError e

    compile `catchError` closeOnError 