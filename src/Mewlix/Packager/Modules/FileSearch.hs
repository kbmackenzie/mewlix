module Mewlix.Packager.Modules.FileSearch
( processSources
, processSource
, validateSource
, validateSources
) where

import Data.Containers.ListUtils (nubOrd)
import Control.Monad ((>=>), unless)
import Conduit
    ( runConduitRes
    , (.|)
    , filterC
    , sinkList
    , sourceDirectoryDeep
    )
import System.FilePath (isExtensionOf)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , makeRelativeToCurrentDirectory
    , canonicalizePath)
import Mewlix.Packager.Maker (ProjectMaker, throwError, liftIO)

findSources :: FilePath -> IO [FilePath]
findSources dir = runConduitRes
     $ sourceDirectoryDeep True dir
    .| filterC (isExtensionOf "mews")
    .| sinkList

processSource :: FilePath -> ProjectMaker [FilePath]
processSource = liftIO . canonicalizePath >=> \path -> do
    isDirectory <- liftIO (doesDirectoryExist path)
    if isDirectory
        then liftIO (findSources path)
        else return [path]

processSources :: [FilePath] -> ProjectMaker [FilePath]
processSources paths = do
    let makeLocal = liftIO . makeRelativeToCurrentDirectory
    sourceBundle <- mapM processSource paths
    sourceFiles  <- mapM makeLocal (concat sourceBundle)
    (return . nubOrd) sourceFiles

validateSource :: FilePath -> ProjectMaker ()
validateSource path = do
    fileExists <- liftIO (doesFileExist path)
    unless fileExists $
        throwError $ concat [ "Couldn't find file ", show path, "!" ]

validateSources :: [FilePath] -> ProjectMaker ()
validateSources = mapM_ validateSource
