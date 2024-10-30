{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Create
( createProject
) where

import Mewlix.Packager.Type (Packager, throwError, liftIO)
import Mewlix.Packager.Log (projectLog)
import Mewlix.Packager.Config (ProjectData(..), projectFieldOrder, projectSourceFilesL)
import Mewlix.Packager.Folder (projectFile, mewlixFolder)
import Mewlix.Utils.Yaml (toPrettyYaml)
import Mewlix.Utils.IO (createDirectory, writeFileBytes)
import System.Directory (doesFileExist)
import Lens.Micro.Platform ((%~))
import System.FilePath (addTrailingPathSeparator)
import qualified Data.ByteString.Char8 as ByteString
import Control.Monad (when)

includeSrc :: ProjectData -> ProjectData
includeSrc = projectSourceFilesL %~ ("src/**/*.mews" :)

createProject :: ProjectData -> Packager ()
createProject = (. includeSrc) $ \projectData -> do
    -- Log project name + a nice message
    projectLog projectData "Creating project file..."

    -- Check for existing project
    exists <- liftIO (doesFileExist projectFile)
    when exists $ do
        throwError "Cannot create project: There's already a project in this directory!"

    -- Create project file:
    let contents = toPrettyYaml projectFieldOrder projectData
    writeFileBytes projectFile contents

    -- Create 'src' directory:
    createDirectory False "./src"

    -- Create .gitignore
    let ignorePath = addTrailingPathSeparator mewlixFolder ++ "\n"
    writeFileBytes "./.gitignore" (ByteString.pack ignorePath)
