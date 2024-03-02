module Mewlix.Project.Assets.Find
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
import Mewlix.Project.Maker (ProjectMaker, throwError, liftIO)
import Control.Monad.IO.Class (MonadIO)

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

processAssets :: (MonadIO m) => [FilePath] -> m [FilePath]
processAssets paths = do
    let makeLocal = liftIO . makeRelativeToCurrentDirectory
    assetBundle <- mapM processAsset paths
    assets      <- mapM makeLocal (concat assetBundle)
    (return . nubOrd) assets

validateAsset :: FilePath -> ProjectMaker ()
validateAsset path = do
    fileExists <- liftIO (doesFileExist path)
    unless fileExists $
        throwError $ concat [ "Couldn't find asset ", show path, "!" ]

validateAssets :: [FilePath] -> ProjectMaker ()
validateAssets = mapM_ validateAsset
