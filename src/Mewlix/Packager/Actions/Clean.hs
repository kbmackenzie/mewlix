{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Clean
( cleanProject
) where

import Mewlix.Packager.Maker (ProjectMaker, liftIO, throwError)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Packager.Data.Types (ProjectData)
import Mewlix.Packager.Log (projectLog)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import Control.Monad (when)

-- Attempt removal exactly three times -- because of this:
-- https://github.com/haskell/directory/pull/108

-- If removal fails all three times, give up entirely.

cleanProject :: ProjectData -> ProjectMaker ()
cleanProject projectData = do
    let folder = outputFolder

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
        projectLog projectData "Cleaning output folder..."
        clean 0
