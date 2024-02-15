{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Actions.Package
( packageProject
) where

import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Project.Maker (ProjectMaker)
import Mewlix.Project.Actions.Build (buildProject)
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Project.Log (projectLog)
import System.Directory (doesDirectoryExist)
import Codec.Archive.Zip
    ( ZipArchive
    , sinkEntry
    , mkEntrySelector
    , CompressionMethod(Deflate)
    , createArchive
    )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import Conduit (sourceFile, sourceDirectoryDeep, runConduitRes, sinkList, (.|))
import System.FilePath (makeRelative, makeValid, replaceExtension)
import qualified Data.Text as Text

packageFolder :: FilePath -> ZipArchive ()
packageFolder folder = do
    let copy :: FilePath -> ZipArchive ()
        copy filepath = do
            let name = makeRelative folder filepath
            selector <- mkEntrySelector name
            sinkEntry Deflate (sourceFile filepath) selector

    filepaths <- liftIO $ runConduitRes
         $ sourceDirectoryDeep True folder
        .| sinkList
    mapM_ copy filepaths

createPackage :: (MonadIO m) => ProjectData -> m ()
createPackage projectData = do
    let name = Text.unpack (projectName projectData)
    let packageName = makeValid name `replaceExtension` ".zip"

    liftIO $ createArchive packageName (packageFolder outputFolder)

packageProject :: ProjectData -> ProjectMaker ()
packageProject projectData = do
    exists <- liftIO (doesDirectoryExist outputFolder)
    unless exists $
        buildProject projectData

    projectLog projectData "Packaging project..."
    createPackage projectData
