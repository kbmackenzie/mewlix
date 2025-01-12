module Mewlix.Packager.Actions.Init
( initProject
) where

import Mewlix.Packager.Type (Packager, throwError, liftIO)
import Mewlix.Packager.Log (initLog)
import Mewlix.Packager.Config (ProjectConfig(..), projectFieldOrder, projectSourceFilesL)
import Mewlix.Packager.Environment (projectFile, mewlixFolder)
import Mewlix.Utils.Yaml (toPrettyYaml)
import Mewlix.Utils.IO (createDirectory, writeFileBytes)
import System.Directory (doesFileExist)
import Lens.Micro.Platform ((%~))
import System.FilePath (addTrailingPathSeparator)
import qualified Data.ByteString.Char8 as ByteString
import Control.Monad (when)
import Control.Arrow ((>>>))

includeSrc :: ProjectConfig -> ProjectConfig
includeSrc = projectSourceFilesL %~ ("src/**/*.mews" :)

initProject :: ProjectConfig -> Packager ()
initProject = includeSrc >>> \config -> do
    -- Project initialization message.
    initLog config

    -- Check for existing project
    exists <- liftIO (doesFileExist projectFile)
    when exists $ do
        throwError "Cannot create project: There's already a project in this directory!"

    -- Create project file:
    let contents = toPrettyYaml projectFieldOrder config
    writeFileBytes projectFile contents

    -- Create 'src' directory:
    createDirectory False "./src"

    -- Create .gitignore
    let ignorePath = addTrailingPathSeparator mewlixFolder ++ "\n"
    writeFileBytes "./.gitignore" (ByteString.pack ignorePath)
