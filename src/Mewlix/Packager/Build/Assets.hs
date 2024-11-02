{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Build.Assets
( copyAssets
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Config (ProjectConfig(..))
import Mewlix.Packager.Environment (buildFolder)
import Mewlix.Packager.Log (projectLog)
import Mewlix.Utils.IO (safelyRun, copyFileSafe, createDirectory, compareFileMods)
import Mewlix.Utils.Show (showT)
import System.FilePath ((</>), takeDirectory)
import System.FilePattern.Directory (getDirectoryFiles)
import Control.Monad (when)

copyAsset :: ProjectConfig -> FilePath -> Packager ()
copyAsset config asset = do
    let prepareDirectory :: FilePath -> Packager ()
        prepareDirectory = createDirectory True . takeDirectory

    let output = buildFolder </> asset
    prepareDirectory output

    shouldCopy <- maybe True (== GT) <$> compareFileMods asset output
    when shouldCopy $ do
        projectLog config ("Copying asset " <> showT asset)
        copyFileSafe asset output

findAssets :: ProjectConfig -> Packager [FilePath]
findAssets config = do
    let patterns = projectAssets config
    safelyRun (getDirectoryFiles "." patterns) "couldn't get asset files"

copyAssets :: ProjectConfig -> Packager ()
copyAssets config = do
    assets <- findAssets config
    mapM_ (copyAsset config) assets
