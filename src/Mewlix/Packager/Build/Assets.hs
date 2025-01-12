module Mewlix.Packager.Build.Assets
( copyAssets
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Config (ProjectConfig(..), ProjectMode(..))
import Mewlix.Packager.Environment (buildFolder)
import Mewlix.Packager.Log (logMessage)
import Mewlix.Utils.IO (safelyRun, copyFileSafe, createDirectory, compareFileMods)
import System.FilePath ((</>), takeDirectory, normalise, splitPath, equalFilePath)
import System.FilePattern.Directory (getDirectoryFiles)
import Control.Monad (when, unless)
import Control.Arrow ((>>>))

reserved :: ProjectMode -> [FilePath]
reserved mode = case mode of
    Console -> ["index.html", "style.css", "init.js"]
    Graphic -> ["index.html", "style.css", "init.js"]
    Node    -> ["init.js"]

isOkayAssetPath :: ProjectMode -> FilePath -> Bool
isOkayAssetPath mode = normalise >>> do
    let isInCore :: FilePath -> Bool
        isInCore path = case splitPath path of
            []    -> False
            (x:_) -> equalFilePath x "core"

    let isReserved :: FilePath -> Bool
        isReserved path = any (equalFilePath path) (reserved mode)

    \path -> not (isInCore path || isReserved path)

copyAsset :: ProjectConfig -> FilePath -> Packager ()
copyAsset config asset = do
    let output = buildFolder </> asset
    createDirectory True (takeDirectory output)

    let mode = projectMode config
    unless (isOkayAssetPath mode asset) . logMessage config . concat $
            ["Asset path ", show output, " likely conflicts with core template asset."]

    isOutdated <- maybe True (== GT) <$> compareFileMods asset output
    when isOutdated $ do
        logMessage config ("Copying asset " ++ show asset)
        copyFileSafe asset output

findAssets :: ProjectConfig -> Packager [FilePath]
findAssets config = do
    let patterns = projectAssets config
    safelyRun (getDirectoryFiles "." patterns) "couldn't get asset files"

copyAssets :: ProjectConfig -> Packager ()
copyAssets config = do
    assets <- findAssets config
    mapM_ (copyAsset config) assets
