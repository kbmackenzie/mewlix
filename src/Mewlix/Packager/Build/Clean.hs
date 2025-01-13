module Mewlix.Packager.Build.Clean
( clean
) where

import Mewlix.Packager.Type (Packager, liftIO, throwError)
import Mewlix.Packager.Environment (mewlixFolder)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import Control.Monad (when)

clean :: Packager ()
clean = do
    let folder = mewlixFolder

    -- Attempt removal exactly three times -- because of this:
    -- https://github.com/haskell/directory/pull/108

    -- If removal fails all three times, give up entirely.
    let rm :: Int -> Packager ()
        rm attempt
            | attempt > 3 = throwError "Critical error: Couldn't remove output folder!"
            | otherwise   = do
                liftIO (removeDirectoryRecursive folder)
                stillExists <- liftIO (doesDirectoryExist folder)

                when stillExists $ rm (attempt + 1)

    exists <- liftIO (doesDirectoryExist folder)
    when exists $ rm 0

