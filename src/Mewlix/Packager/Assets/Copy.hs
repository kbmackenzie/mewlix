module Mewlix.Packager.Assets.Copy
( copyAssets
) where

import Mewlix.Packager.Data.Types (ProjectData(..))
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Packager.Maker (PackageMaker, throwError)
import Mewlix.Packager.Assets.Find (processAssets, validateAssets)
import Mewlix.Utils.IO (copyFileSafe, createDirectory)
import System.FilePath
    ( (</>)
    , isRelative
    , takeDirectory
    )

copyAsset :: FilePath -> PackageMaker ()
copyAsset inputPath = do
    let prepareDirectory :: FilePath -> PackageMaker ()
        prepareDirectory = createDirectory True . takeDirectory

    outputPath <- if isRelative inputPath
        then return (outputFolder </> inputPath)
        else throwError $ concat
            [ "Asset file path cannot be made relative to current directory: "
            , show inputPath
            , "!\nPlease use relative paths without indirections!" ]
    prepareDirectory outputPath
    copyFileSafe inputPath outputPath

copyAssets :: ProjectData -> PackageMaker ()
copyAssets projectData = do
    assets <- processAssets (projectAssets projectData)
    validateAssets assets
    mapM_ copyAsset assets
