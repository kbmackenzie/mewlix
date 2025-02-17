module Mewlix.Packager.Build.Assets
( copyAssets
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Config (ProjectConfig(..), ProjectMode(..))
import Mewlix.Packager.Environment (buildFolder)
import Mewlix.Packager.Log (logMessage, logWarning)
import Mewlix.Utils.IO (safelyRun, copyFileSafe, createDirectory, compareFileMods)
import System.FilePath ((</>), takeDirectory, normalise, splitPath, equalFilePath)
import System.FilePattern.Directory (getDirectoryFiles)
import Control.Monad (when)
import Control.Arrow ((>>>))

templateAssets :: ProjectMode -> [FilePath]
templateAssets mode = case mode of
    Console -> ["index.html", "style.css", "init.js"]
    Graphic -> ["index.html", "style.css", "init.js"]
    Node    -> ["init.js"]
    Blank   -> ["init.js"]

isReserved :: ProjectMode -> FilePath -> Bool
isReserved mode = normalise >>> do
    let isInCore :: FilePath -> Bool
        isInCore path = case splitPath path of
            []    -> False
            (x:_) -> any (equalFilePath x) ["core", "yarnball"]

    let isTemplateAsset :: FilePath -> Bool
        isTemplateAsset path = any (equalFilePath path) (templateAssets mode)

    \path -> isInCore path || isTemplateAsset path

copyAsset :: ProjectConfig -> FilePath -> Packager ()
copyAsset config asset = do
    let output = buildFolder </> asset
    createDirectory True (takeDirectory output)

    let mode = projectMode config
    when (isReserved mode asset) . logWarning . concat $
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
