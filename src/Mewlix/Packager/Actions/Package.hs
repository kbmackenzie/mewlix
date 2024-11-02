{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Package
( packageProject
) where

import Mewlix.Packager.Config (ProjectConfig(..))
import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Actions.Build (buildProject)
import Mewlix.Packager.Environment (buildFolder)
import Mewlix.Packager.Log (projectLog)
import System.Directory (doesDirectoryExist)
import Codec.Archive.Zip
    ( ZipArchive
    , packDirRecur
    , mkEntrySelector
    , CompressionMethod(Deflate)
    , createArchive
    )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import System.FilePath (makeValid, replaceExtension)
import qualified Data.Text as Text

packageDirectory :: FilePath -> ZipArchive ()
packageDirectory = packDirRecur Deflate mkEntrySelector

createPackage :: (MonadIO m) => ProjectConfig -> m ()
createPackage config = do
    let name = Text.unpack (projectName config)
    let packageName = makeValid name `replaceExtension` ".zip"

    liftIO $ createArchive packageName (packageDirectory buildFolder)

packageProject :: ProjectConfig -> Packager ()
packageProject config = do
    exists <- liftIO (doesDirectoryExist buildFolder)
    unless exists $
        buildProject config

    projectLog config "Packaging project..."
    createPackage config
