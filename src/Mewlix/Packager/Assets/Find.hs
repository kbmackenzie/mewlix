module Mewlix.Packager.Assets.Find
( processAssets
, validateAssets
) where

import Data.Containers.ListUtils (nubOrd)
import Control.Monad ((>=>), unless)
import Conduit
    ( runConduitRes
    , (.|)
    , sinkList
    , sourceDirectoryDeep
    )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , makeRelativeToCurrentDirectory
    , canonicalizePath
    )
import Mewlix.Packager.Maker (PackageMaker, throwError, liftIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Exception (IOException)

findAssets :: (MonadIO m) => FilePath -> m [FilePath]
findAssets path = liftIO . runConduitRes
     $ sourceDirectoryDeep True path
    .| sinkList

processAsset :: (MonadIO m) => FilePath -> m [FilePath]
processAsset = liftIO . canonicalizePath >=> \path -> do
    isDirectory <- liftIO (doesDirectoryExist path)
    if isDirectory
        then liftIO (findAssets path)
        else return [path]

processAssets :: [FilePath] -> PackageMaker [FilePath]
processAssets paths = do
    let getAssets :: IO [FilePath]
        getAssets = do
            let makeLocal = liftIO . makeRelativeToCurrentDirectory
            assetBundle <- mapM processAsset paths
            mapM makeLocal (concat assetBundle)

    assets <- liftIO getAssets -- todo: error-handle here. >:/ (and everywhere else.)
    (return . nubOrd) assets

validateAsset :: FilePath -> PackageMaker ()
validateAsset path = do
    fileExists <- liftIO (doesFileExist path)
    unless fileExists $
        throwError $ concat [ "Couldn't find asset ", show path, "!" ]

validateAssets :: [FilePath] -> PackageMaker ()
validateAssets = mapM_ validateAsset
