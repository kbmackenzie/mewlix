{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Build
( project
, buildProject
) where

import Mewlix.Project.Make (ProjectMaker, liftIO)
import Mewlix.Project.Read (readProject)
import Mewlix.Project.Data (ProjectData(..))
import Mewlix.Project.Modules (compileModules)
import Mewlix.Project.Template (createFromTemplate)
import qualified Data.List as List
import qualified Mewlix.Utils.FileIO as FileIO

buildProject :: ProjectData -> ProjectMaker ()
buildProject projectData = do
    -- Template:
    createFromTemplate (projectMode projectData)

    -- Modules:
    compiledModules <- compileModules projectData
    scriptList compiledModules

scriptList :: [FilePath] -> ProjectMaker ()
scriptList paths = do
    let targetPath = "output/mewlix/core/script-list"
    let scripts = List.intercalate "\n" paths
    liftIO $ FileIO.writeFileS targetPath scripts

project :: ProjectMaker ()
project = readProject >>= buildProject
