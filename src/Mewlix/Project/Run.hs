{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Run
( make
, singletonProject
) where

import Mewlix.Project.Make
    ( ProjectMaker(..)
    , Language(..)
    , projectMakeJS
    )
import Mewlix.Project.Data (ProjectData(..))
import Mewlix.Project.Mode (ProjectMode(..))
import Mewlix.Project.Init (project, initProject)
import qualified Mewlix.Utils.Logging as Logging
import qualified Data.Text as Text
import System.FilePath (takeBaseName)

type ProjectCallback = ProjectMaker () -> IO (Either String ())

makeRun :: ProjectCallback -> ProjectMaker () -> IO ()
makeRun callback action = callback action >>= \case
    (Left err) -> Logging.writeStderr err
    (Right _ ) -> return ()

make :: Language -> IO ()
make Javascript = makeRun projectMakeJS project

singletonProject :: Language -> FilePath -> IO ()
singletonProject Javascript path = makeRun projectMakeJS (initProject projectData)
    where projectData = ProjectData
            { projectName           = (Text.pack . takeBaseName) path
            , projectDescription    = mempty
            , projectMode           = Console
            , projectSourceFiles    = [path]
            , projectSpecialImports = mempty
            , projectFlags          = mempty }
