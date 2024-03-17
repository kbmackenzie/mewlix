module Mewlix.Project.Data.Read
( readProject
) where

import Mewlix.Project.Maker (ProjectMaker, liftIO, throwError)
import Mewlix.Project.Folder (projectFile)
import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Utils.FileIO (readFileB)
import Mewlix.Utils.Yaml (readYaml)
import System.FilePath (takeFileName)
import System.Directory (doesFileExist)
import Control.Monad (unless)

readProject :: ProjectMaker ProjectData
readProject = do
    hasProject <- liftIO $ doesFileExist projectFile
    let projectFileName = takeFileName projectFile

    unless hasProject $
        (throwError . concat) [ "Couldn't find a ", show projectFileName, " file in the current directory!" ]

    readFileB projectFile >>= \contents -> case readYaml contents of
        (Left err)  -> (throwError . concat) [ "Couldn't parse ", show projectFileName, " file: ", err ]
        (Right dat) -> return dat
