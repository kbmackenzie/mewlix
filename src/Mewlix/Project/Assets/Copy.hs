module Mewlix.Project.Assets.Copy
( copyAssets
) where

import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Project.Maker (ProjectMaker, throwError, liftIO)
import Mewlix.Project.Assets.Find (processAssets, validateAssets)
import Mewlix.Utils.FileIO (copyFile)
import System.Directory
    ( makeRelativeToCurrentDirectory
    , createDirectoryIfMissing
    )
import System.FilePath
    ( (</>)
    , isRelative
    , takeDirectory
    )

copyAsset :: FilePath -> ProjectMaker ()
copyAsset inputPath = do
    let prepareDirectory :: FilePath -> ProjectMaker ()
        prepareDirectory = liftIO . createDirectoryIfMissing True . takeDirectory

    outputPath <- do
        relative <- liftIO (makeRelativeToCurrentDirectory inputPath)
        if isRelative relative
            then return (outputFolder </> relative)
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
