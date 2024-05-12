module Mewlix.Packager.Assets.Copy
( copyAssets
) where

import Mewlix.Packager.Data.Types (ProjectData(..))
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Packager.Maker (ProjectMaker, throwError, liftIO)
import Mewlix.Packager.Assets.Find (processAssets, validateAssets)
import Mewlix.Utils.FileIO (copyFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
    ( (</>)
    , isRelative
    , takeDirectory
    )

copyAsset :: FilePath -> ProjectMaker ()
copyAsset inputPath = do
    let prepareDirectory :: FilePath -> ProjectMaker ()
        prepareDirectory = liftIO . createDirectoryIfMissing True . takeDirectory

    outputPath <- if isRelative inputPath
        then return (outputFolder </> inputPath)
        else throwError $ concat
            [ "Asset file path cannot be made relative to current directory: "
            , show inputPath
            , "!\nPlease use relative paths without indirections!" ]

    prepareDirectory outputPath
    copyFile inputPath outputPath

copyAssets :: ProjectData -> ProjectMaker ()
copyAssets projectData = do
    assets <- processAssets (projectAssets projectData)
    validateAssets assets
    mapM_ copyAsset assets
