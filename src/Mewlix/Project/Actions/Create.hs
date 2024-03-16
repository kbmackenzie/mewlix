{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Actions.Create
( createProject
) where

import Mewlix.Project.Data.Types (ProjectData(..), projectFieldOrder, projectSourceFilesL)
import Mewlix.Project.Log (projectLog)
import Mewlix.Project.Folder (projectFile)
import Mewlix.Utils.Yaml (prettyYaml)
import Mewlix.Utils.FileIO (writeFileB)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory (createDirectoryIfMissing)
import Lens.Micro.Platform ((%~))

includeSrc :: ProjectData -> ProjectData
includeSrc = projectSourceFilesL %~ ("src/" :)

createProject :: (MonadIO m) => ProjectData -> m ()
createProject = (. includeSrc) $ \projectData -> do
    -- Log project name + a nice message
    projectLog projectData "Creating project file..."

    -- Create project file:
    let contents = prettyYaml projectFieldOrder projectData
    writeFileB projectFile contents

    -- Create 'src' directory:
    liftIO (createDirectoryIfMissing False "./src")

    -- Create .gitignore
    writeFileB "./.gitignore" "output/"
