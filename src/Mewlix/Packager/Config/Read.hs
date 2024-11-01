module Mewlix.Packager.Config.Read
( readProject
) where

import Mewlix.Packager.Type (Packager, liftIO, throwError)
import Mewlix.Packager.Environment (projectFile)
import Mewlix.Packager.Config.Types (ProjectConfig(..))
import Mewlix.Utils.IO (readFileBytes)
import Mewlix.Utils.Yaml (parseYaml)
import System.FilePath (takeFileName)
import System.Directory (doesFileExist)
import Control.Monad (unless)

readProject :: Packager ProjectConfig
readProject = do
    hasProject <- liftIO $ doesFileExist projectFile
    let projectFileName = takeFileName projectFile

    unless hasProject $ do
        throwError $ concat [ "Couldn't find a ", show projectFileName, " file in the current directory!" ]

    readFileBytes projectFile >>= \contents -> case parseYaml contents of
        (Left err)  -> (throwError . concat) [ "Couldn't parse ", show projectFileName, " file: ", err ]
        (Right dat) -> return dat
