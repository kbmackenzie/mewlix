{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Read
( readProject
) where

import Conduit (runConduitRes, (.|), sourceDirectory, filterC, headC)
import Mewlix.Project.Make (ProjectMaker, liftIO, throwError)
import Mewlix.Project.Data (ProjectData(..))
import Mewlix.Utils.Yaml (readYaml)
import System.FilePath (isExtensionOf)

findProject :: ProjectMaker FilePath
findProject = do
    let search :: IO (Maybe FilePath) 
        search = runConduitRes
             $ sourceDirectory "."
            .| filterC (isExtensionOf "mewlix")
            .| headC

    liftIO search >>= \case
        (Just path) -> return path
        Nothing     -> throwError "Couldn't find a '.mewlix' project file the in current directory!"

readProject :: ProjectMaker ProjectData
readProject = do
    path <- findProject
    readYaml path >>= \case
        (Left err)  -> throwError $ concat
            [ "Couldn't parse project file: \"", path, "\":",  show err ]
        (Right dat) -> return dat
