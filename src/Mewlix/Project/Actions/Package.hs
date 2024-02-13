{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Actions.Package
( packageProject
) where

import Mewlix.Project.Data.Types (ProjectData)
import Mewlix.Project.Maker (ProjectMaker)
import Mewlix.Project.Actions.Build (buildProject)
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Project.Log (projectLog)
import System.Directory (doesDirectoryExist)
import Codec.Archive.Zip
    ( createArchive
    , packDirRecur
    , CompressionMethod(Deflate)
    , mkEntrySelector
    )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)

createPackage :: (MonadIO m) => FilePath -> m ()
createPackage targetPath = liftIO (createArchive targetPath packDirectory)
    where packDirectory = packDirRecur Deflate mkEntrySelector outputFolder

packageProject :: ProjectData -> ProjectMaker ()
packageProject projectData = do
    exists <- liftIO (doesDirectoryExist outputFolder)
    unless exists $
        buildProject projectData

    projectLog projectData "Packaging project..."
    createPackage outputFolder
