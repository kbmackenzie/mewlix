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
import Data.Maybe (fromMaybe)

readProject :: Maybe FilePath -> Packager ProjectConfig
readProject customPath = do
    let config = fromMaybe projectFile customPath

    hasConfig <- liftIO $ doesFileExist config
    let name = takeFileName config

    unless hasConfig $ do
        throwError $ concat [ "Couldn't find a ", show name, " file in the current directory!" ]

    readFileBytes config >>= \contents -> case parseYaml contents of
        (Left err)  -> (throwError . concat) [ "Couldn't parse ", show name, " file: ", err ]
        (Right dat) -> return dat
