{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Actions.Create
( createProject
) where

import Mewlix.Project.Data.Types (ProjectData(..), projectFieldOrder)
import Mewlix.Utils.Yaml (prettyYaml)
import Mewlix.Utils.FileIO (writeFileB)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as Text
import System.FilePath (replaceExtension, makeValid)

createProject :: (MonadIO m) => ProjectData -> m ()
createProject projectData = do
    let name = (makeValid . Text.unpack . projectName) projectData
    let path = replaceExtension ("./" <> name) "mewlix"
    let contents = prettyYaml projectFieldOrder projectData
    writeFileB path contents
