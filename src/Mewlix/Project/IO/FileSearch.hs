module Mewlix.Project.IO.FileSearch
( processSources
, validateFile
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
    ( canonicalizePath
    , makeRelativeToCurrentDirectory
    , doesDirectoryExist
    , doesFileExist
    )
import Mewlix.Project.Make (ProjectMaker, throwError, liftIO)

localRelative :: FilePath -> IO FilePath
localRelative = canonicalizePath >=> makeRelativeToCurrentDirectory

findSources :: FilePath -> IO [FilePath]
findSources dir = runConduitRes
     $ sourceDirectoryDeep True dir
    .| filterC (isExtensionOf "mews")
    .| sinkList

processSource :: FilePath -> IO [FilePath]
processSource path = do
    isDirectory <- (localRelative >=> doesDirectoryExist) path
    if isDirectory
        then findSources path
        else return [path]

processSources :: [FilePath] -> IO [FilePath]
processSources paths = do
    sourceFiles <- mapM processSource paths
    (return . nubOrd . concat) sourceFiles

validateFile :: FilePath -> ProjectMaker ()
validateFile path = do
    fileExists <- liftIO (doesFileExist path)
    unless fileExists $
        throwError $ mconcat [ "Couldn't find file \"", path, "\"!" ]
