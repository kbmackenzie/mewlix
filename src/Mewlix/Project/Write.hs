{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Write
(
) where

import Mewlix.Project.Maker
    ( ProjectMaker(..)
    , liftIO
    , throwError
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Containers.ListUtils (nubOrd)
import Control.Monad ((>=>), when, unless)
import System.FilePath
    ( normalise
    , makeRelative
    , isExtensionOf
    )
import System.Directory
    ( canonicalizePath
    , makeRelativeToCurrentDirectory
    , doesDirectoryExist
    , listDirectory
    )
import qualified Data.List as List

processDirectories :: [FilePath] -> IO [FilePath]
processDirectories = fmap nubOrd . mapM (canonicalizePath >=> makeRelativeToCurrentDirectory)

searchForYarnball :: FilePath -> ProjectMaker [FilePath]
searchForYarnball path = do
    isValid <- liftIO (doesDirectoryExist path)
    unless isValid $
        throwError (concat [ "Path \"", path, "\" is not an existing directory!"])
    filter (isExtensionOf ".mews") <$> liftIO (listDirectory path)
