{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Data.Read
( findProject
, readProject
) where

import Conduit (runConduitRes, (.|), sourceDirectory, filterC, headC)
import Mewlix.Project.Maker (ProjectMaker, liftIO, throwError)
import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Utils.Yaml (readYaml)
import System.FilePath (isExtensionOf)

findProject :: ProjectMaker (Maybe FilePath)
findProject = liftIO $ runConduitRes
     $ sourceDirectory "."
    .| filterC (isExtensionOf "mewlix")
    .| headC

readProject :: ProjectMaker ProjectData
readProject = do
    path <- findProject >>= \case
        (Just path) -> return path
        Nothing     -> throwError
            "Couldn't find a '.mewlix' project file the in current directory!"

    readYaml path >>= \case
        (Left err)  -> throwError $ concat
            [ "Couldn't parse project file: \"", path, "\":",  show err ]
        (Right dat) -> return dat
