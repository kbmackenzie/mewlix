{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Package
( packageProject
) where

import Mewlix.Packager.Config.Types (ProjectData(..))
import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Actions.Build (buildProject)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Packager.Log (projectLog)
import System.Directory (doesDirectoryExist)
import Codec.Archive.Zip
    ( ZipArchive
    , packDirRecur
    , mkEntrySelector
    , EntrySelector
    , CompressionMethod(Deflate)
    , createArchive
    )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import System.FilePath (makeValid, replaceExtension, (</>))
import qualified Data.Text as Text

packageDirectory :: FilePath -> ZipArchive ()
packageDirectory directory = do
    let selector :: FilePath -> ZipArchive EntrySelector
        selector = mkEntrySelector . (directory </>)
    packDirRecur Deflate selector directory

createPackage :: (MonadIO m) => ProjectData -> m ()
createPackage projectData = do
    let name = Text.unpack (projectName projectData)
    let packageName = makeValid name `replaceExtension` ".zip"

    liftIO $ createArchive packageName (packageDirectory outputFolder)

packageProject :: ProjectData -> Packager ()
packageProject projectData = do
    exists <- liftIO (doesDirectoryExist outputFolder)
    unless exists $
        buildProject projectData

    projectLog projectData "Packaging project..."
    createPackage projectData
