module Mewlix.Packager.Assets
( copyAssets
) where

import Mewlix.Packager.Type (Packager, throwError)
import Mewlix.Packager.Config (ProjectConfig(..))
import Mewlix.Packager.Folder (buildFolder)
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
        then return (buildFolder </> inputPath)
        else throwError $ concat
            [ "Asset file path cannot be made relative to current directory: "
            , show inputPath
            , "!\nPlease use relative paths without indirections!" ]
    prepareDirectory outputPath
    copyFileSafe inputPath outputPath

findAssets :: ProjectConfig -> Packager [FilePath]
findAssets config = do
    let patterns = projectSourceFiles config
    safelyRun (getDirectoryFiles "." patterns) "couldn't get asset files"

copyAssets :: ProjectConfig -> Packager ()
copyAssets config = do
    assets <- findAssets config
    mapM_ copyAsset assets
