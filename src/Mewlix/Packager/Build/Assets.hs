module Mewlix.Packager.Build.Assets
( copyAssets
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Config (ProjectConfig(..))
import Mewlix.Packager.Environment (buildFolder)
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

    shouldCopy <- maybe True (== GT) <$> compareFileMods inputPath outputPath
    when shouldCopy (copyFileSafe inputPath outputPath)

findAssets :: ProjectConfig -> Packager [FilePath]
findAssets config = do
    let patterns = projectSourceFiles config
    safelyRun (getDirectoryFiles "." patterns) "couldn't get asset files"

copyAssets :: ProjectConfig -> Packager ()
copyAssets config = do
    assets <- findAssets config
    mapM_ copyAsset assets
