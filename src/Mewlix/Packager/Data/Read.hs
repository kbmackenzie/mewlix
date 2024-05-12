module Mewlix.Packager.Data.Read
( readProject
) where

import Mewlix.Packager.Maker (PackageMaker, liftIO, throwError)
import Mewlix.Packager.Folder (projectFile)
import Mewlix.Packager.Data.Types (ProjectData(..))
import Mewlix.Utils.FileIO (readBytes)
import Mewlix.Utils.Yaml (readYaml)
import System.FilePath (takeFileName)
import System.Directory (doesFileExist)
import Control.Monad (unless)

readProject :: PackageMaker ProjectData
readProject = do
    hasProject <- liftIO $ doesFileExist projectFile
    let projectFileName = takeFileName projectFile

    unless hasProject $
        (throwError . concat) [ "Couldn't find a ", show projectFileName, " file in the current directory!" ]

    readBytes projectFile >>= \contents -> case readYaml contents of
        (Left err)  -> (throwError . concat) [ "Couldn't parse ", show projectFileName, " file: ", err ]
        (Right dat) -> return dat
