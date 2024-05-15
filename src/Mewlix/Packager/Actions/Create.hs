{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Create
( createProject
) where

import Mewlix.Packager.Data.Types (ProjectData(..), projectFieldOrder, projectSourceFilesL)
import Mewlix.Packager.Log (projectLog)
import Mewlix.Packager.Folder (projectFile, outputFolder)
import Mewlix.Utils.Yaml (prettyYaml)
import Mewlix.Utils.FileIO (writeBytes)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory (createDirectoryIfMissing)
import Lens.Micro.Platform ((%~))
import System.FilePath (addTrailingPathSeparator)
import qualified Data.ByteString.Char8 as ByteString

includeSrc :: ProjectData -> ProjectData
includeSrc = projectSourceFilesL %~ ("src/" :)

createProject :: (MonadIO m) => ProjectData -> m ()
createProject = (. includeSrc) $ \projectData -> do
    -- Log project name + a nice message
    projectLog projectData "Creating project file..."

    -- Create project file:
    let contents = prettyYaml projectFieldOrder projectData
    writeBytes projectFile contents

    -- Create 'src' directory:
    liftIO (createDirectoryIfMissing False "./src")

    -- Create .gitignore
    let ignorePath = addTrailingPathSeparator outputFolder ++ "\n"
    writeBytes "./.gitignore" (ByteString.pack ignorePath)
