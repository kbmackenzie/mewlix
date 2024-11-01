module Mewlix.Packager.Assets
( copyAssets
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Config (ProjectConfig(..))
import Mewlix.Packager.Folder (buildFolder)
import Mewlix.Utils.IO (safelyRun, copyFileSafe, createDirectory, compareFileMods)
import System.FilePath ((</>), takeDirectory)
import System.FilePattern.Directory (getDirectoryFiles)
import Control.Monad (when)

copyAsset :: FilePath -> Packager ()
copyAsset inputPath = do
    let prepareDirectory :: FilePath -> Packager ()
        prepareDirectory = createDirectory True . takeDirectory

    let outputPath = buildFolder </> inputPath
    prepareDirectory outputPath

    comparison <- compareFileMods inputPath outputPath
    let shouldCopy = maybe True (== GT) comparison
    when shouldCopy (copyFileSafe inputPath outputPath)

findAssets :: ProjectConfig -> Packager [FilePath]
findAssets config = do
    let patterns = projectSourceFiles config
    safelyRun (getDirectoryFiles "." patterns) "couldn't get asset files"

copyAssets :: ProjectConfig -> Packager ()
copyAssets config = do
    assets <- findAssets config
    mapM_ copyAsset assets
