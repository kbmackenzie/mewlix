{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Make
( make
, singletonProject
) where

import Mewlix.Project.ProjectMaker
    ( ProjectMaker(..)
    , Language(..)
    , projectMakeJS
    )
import Mewlix.Project.Data.Types (ProjectData(..), projectSourceFilesL)
import Mewlix.Project.Actions.Build (buildProject)
import Mewlix.Project.Data.Read (readProject)
import qualified Mewlix.Utils.Logging as Logging
import Lens.Micro.Platform (over)

type ProjectCallback = ProjectMaker () -> IO (Either String ())

makeRun :: ProjectCallback -> ProjectMaker () -> IO ()
makeRun callback action = callback action >>= \case
    (Left err) -> Logging.writeStderr err
    (Right _ ) -> return ()

make :: Language -> IO ()
make Javascript = makeRun projectMakeJS (readProject >>= buildProject)

singletonProject :: Language -> [FilePath] -> ProjectData -> IO ()
singletonProject Javascript files = makeRun projectMakeJS . buildProject . addFiles
    where addFiles = over projectSourceFilesL (files ++)
