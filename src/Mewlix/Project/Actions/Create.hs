{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Actions.Create
( createProject
) where

import Mewlix.Project.Data.Types (ProjectData(..), projectFieldOrder)
import Mewlix.Project.Log (projectLog)
import Mewlix.Utils.Yaml (prettyYaml)
import Mewlix.Utils.FileIO (writeFileB)
import Mewlix.Utils.Show (showT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as Text
import System.FilePath (replaceExtension, makeValid)
import System.Directory (createDirectoryIfMissing)

createProject :: (MonadIO m) => ProjectData -> m ()
createProject projectData = do
    -- Log project name + a nice message
    projectLog projectData "Creating project file..."

    -- Create project file:
    let name = (makeValid . Text.unpack . projectName) projectData
    let path = replaceExtension ("./" <> name) "mewlix"
    let contents = prettyYaml projectFieldOrder projectData
    writeFileB path contents

    -- Create 'src' directory:
    liftIO (createDirectoryIfMissing False "./src")
