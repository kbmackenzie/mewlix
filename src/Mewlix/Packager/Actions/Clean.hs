{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Clean
( cleanProject
) where

import Mewlix.Packager.Type (Packager, liftIO, throwError)
import Mewlix.Packager.Folder (mewlixFolder)
import Mewlix.Packager.Config (ProjectConfig)
import Mewlix.Packager.Log (projectLog)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import Control.Monad (when)

-- Attempt removal exactly three times -- because of this:
-- https://github.com/haskell/directory/pull/108

-- If removal fails all three times, give up entirely.

cleanProject :: ProjectConfig -> Packager ()
cleanProject config = do
    let folder = mewlixFolder

    let clean :: Int -> Packager ()
        clean attempt
            | attempt > 3 = throwError "Critical error: Couldn't remove output folder!"
            | otherwise   = do
                liftIO (removeDirectoryRecursive folder)
                stillExists <- liftIO (doesDirectoryExist folder)

                when stillExists $
                    clean (attempt + 1)

    exists <- liftIO (doesDirectoryExist folder)
    when exists $ do
        projectLog config "Cleaning output folder..."
        clean 0
