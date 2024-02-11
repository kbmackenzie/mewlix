{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Init
( initProject
) where

import Conduit
    ( runConduitRes
    , (.|)
    , sourceDirectory
    , filterC
    , headC
    )
import Mewlix.Project.Make
    ( ProjectMaker
    , liftIO
    , throwError
    )
import Mewlix.Project.Data (ProjectData(..))
import Mewlix.Project.Modules (compileModules)
import Mewlix.Project.Template (createFromTemplate)
import Mewlix.Utils.Yaml (readYaml)
import System.FilePath (isExtensionOf)
import qualified Data.List as List
import qualified Mewlix.Utils.FileIO as FileIO

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

initProject :: ProjectMaker ()
initProject = do
    projectData <- readProject
    createFromTemplate (projectMode projectData)

    compiledModules <- compileModules projectData
    scriptList compiledModules

scriptList :: [FilePath] -> ProjectMaker ()
scriptList paths = do
    let targetPath = "output/mewlix/core/script-list"
    let scripts = List.intercalate "\n" paths
    liftIO $ FileIO.writeFileS targetPath scripts
