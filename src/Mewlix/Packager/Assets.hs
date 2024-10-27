module Mewlix.Packager.Assets
( copyAssets
) where

import Mewlix.Packager.Type (Packager, throwError)
import Mewlix.Packager.Config (ProjectData(..))
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Utils.IO (safelyRun, copyFileSafe, createDirectory)
import System.FilePath
    ( (</>)
    , isRelative
    , takeDirectory
    )
import System.FilePattern.Directory (getDirectoryFiles)

copyAsset :: FilePath -> Packager ()
copyAsset inputPath = do
    let prepareDirectory :: FilePath -> Packager ()
        prepareDirectory = createDirectory True . takeDirectory

    outputPath <- if isRelative inputPath
        then return (outputFolder </> inputPath)
        else throwError $ concat
            [ "Asset file path cannot be made relative to current directory: "
            , show inputPath
            , "!\nPlease use relative paths without indirections!" ]
    prepareDirectory outputPath
    copyFileSafe inputPath outputPath

findAssets :: ProjectData -> Packager [FilePath]
findAssets projectData = do
    let patterns = projectSourceFiles projectData
    safelyRun (getDirectoryFiles "." patterns) "couldn't get asset files"

copyAssets :: ProjectData -> Packager ()
copyAssets projectData = do
    assets <- findAssets projectData
    mapM_ copyAsset assets
