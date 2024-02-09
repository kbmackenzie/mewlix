module Mewlix.Project.IO.FileSearch
( processSources
) where

import Data.Containers.ListUtils (nubOrd)
import Control.Monad ((>=>))
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
    )

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
