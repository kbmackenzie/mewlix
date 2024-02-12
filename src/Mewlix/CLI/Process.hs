module Mewlix.CLI.Process
( runBuild
, runClean
) where

import Mewlix.Project (Language(..), make, singletonProject)
import Mewlix.Project.ProjectData (projectDataEmpty)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import qualified Mewlix.Utils.Logging as Logging
import Control.Monad (when)

-- Language will be a command-line parameter when more language options are added, if they ever are.
-- For now, however; this.
runBuild :: [FilePath] -> IO ()
runBuild [] = make Javascript 
runBuild xs = singletonProject Javascript xs projectDataEmpty

runClean :: IO ()
runClean = do
    let dir = "output"
    removeDirectoryRecursive dir

    stillExists <- doesDirectoryExist dir
    when stillExists $ do
        Logging.writeStderr "'clean' operation failed: Couldn't remove \"output\" directory!"
        return ()
