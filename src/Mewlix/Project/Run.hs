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
import Mewlix.Project.Data (ProjectData(..), projectSourceFilesL)
import Mewlix.Project.Build (project, buildProject)
import qualified Mewlix.Utils.Logging as Logging
import Lens.Micro.Platform (over)

type ProjectCallback = ProjectMaker () -> IO (Either String ())

makeRun :: ProjectCallback -> ProjectMaker () -> IO ()
makeRun callback action = callback action >>= \case
    (Left err) -> Logging.writeStderr err
    (Right _ ) -> return ()

make :: Language -> IO ()
make Javascript = makeRun projectMakeJS project

singletonProject :: Language -> [FilePath] -> ProjectData -> IO ()
singletonProject Javascript files = makeRun projectMakeJS . buildProject . addFiles
    where addFiles = over projectSourceFilesL (files ++)
