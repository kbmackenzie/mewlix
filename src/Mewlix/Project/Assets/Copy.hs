module Mewlix.Project.Assets.Copy
( copyAssets
) where

import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Project.Maker (ProjectMaker, liftIO)
import Mewlix.Project.Assets.Find (processAssets, validateAssets)
import Mewlix.Utils.FileIO (copyFile)
import System.Directory
    ( makeRelativeToCurrentDirectory
    , createDirectoryIfMissing
    )
import System.FilePath
    ( (</>)
    , isAbsolute
    , takeDirectory
    , dropDrive
    )

copyAsset :: FilePath -> ProjectMaker ()
copyAsset inputPath = do
    let prepareDirectory :: FilePath -> ProjectMaker ()
        prepareDirectory = liftIO . createDirectoryIfMissing True . takeDirectory

    outputPath <- liftIO $ do
        relative <- makeRelativeToCurrentDirectory inputPath
        let folder = outputFolder
        return $ if isAbsolute relative
            then folder </> dropDrive relative
            else folder </> relative

    prepareDirectory outputPath
    copyFile inputPath outputPath

copyAssets :: ProjectData -> ProjectMaker ()
copyAssets projectData = do
    assets <- processAssets (projectAssets projectData)
    validateAssets assets
    mapM_ copyAsset assets
