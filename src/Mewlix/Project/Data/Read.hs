{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Data.Read
( readProject
) where

import Mewlix.Project.Maker (ProjectMaker, liftIO, throwError)
import Mewlix.Project.Folder (projectFile)
import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Utils.Yaml (readYaml)
import System.Directory (doesFileExist)
import Control.Monad (unless)

readProject :: ProjectMaker ProjectData
readProject = do
    hasProject <- liftIO $ doesFileExist projectFile
    unless hasProject $
        throwError
            "Couldn't find a project file the in current directory!"

    readYaml projectFile >>= \case
        (Left err)  -> throwError ("Couldn't parse project file: " ++  show err)
        (Right dat) -> return dat
