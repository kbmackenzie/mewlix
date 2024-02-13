{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Actions.Clean
( cleanProject
) where

import Mewlix.Project.Maker (ProjectMaker, liftIO, throwError)
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Project.Data.Read (readProject)
import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Project.Log (projectLog)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import Control.Monad (when)

-- Attempt removal exactly three times -- because of this:
-- https://github.com/haskell/directory/pull/108

-- If removal fails all three times, give up entirely.

cleanProject :: ProjectMaker ()
cleanProject = do
    let folder = outputFolder
    projectData <- readProject

    let clean :: Int -> ProjectMaker ()
        clean attempt
            | attempt > 3 = throwError "Critical error: Couldn't remove output folder!"
            | otherwise   = do
                liftIO (removeDirectoryRecursive folder)
                stillExists <- liftIO (doesDirectoryExist folder)

                when stillExists $
                    clean (attempt + 1)

    exists <- liftIO (doesDirectoryExist folder)
    when exists $ do
        projectLog projectData "Cleaning project file..."
        clean 0
