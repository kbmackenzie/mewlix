{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Make
( Action(..)
, make
, make'
-- Re-exports:
, Language(..)
) where

import Mewlix.Project.Maker
    ( ProjectMaker(..)
    , Language(..)
    , projectMakeJS
    )
import Mewlix.Project.Data.Types
    ( ProjectData(..)
    , ProjectTransform
    , transformProject
    , defaultProject
    )
-- Actions:
import Mewlix.Project.Actions.Build (buildProject)
import Mewlix.Project.Actions.Clean (cleanProject)
import Mewlix.Project.Actions.Package (packageProject)
import Mewlix.Project.Actions.Create (createProject)
import Mewlix.Project.Actions.Server (runProject)
-- Assorted:
import Mewlix.Project.Data.Read (readProject)
import Mewlix.Logger (LogType(..), logger);
import qualified Data.Text as Text

data Action =
      Build
    | Clean
    | Create
    | Package
    | Run
    deriving (Eq, Ord, Show, Enum, Bounded)

type ActionFunc  = ProjectData -> ProjectMaker ()
type ProjectFunc = ProjectMaker () -> IO (Either String ())

language :: Language -> ProjectFunc
language JavaScript = projectMakeJS

action :: Action -> ActionFunc
action Build    = buildProject
action Clean    = cleanProject
action Create   = createProject
action Package  = packageProject
action Run      = runProject

make :: Bool -> [ProjectTransform] -> Language -> Action -> IO ()
make readProjectFile transforms langOption actOption = execute
    readProjectFile
    (language langOption)
    (action actOption . transformProject transforms)

make' :: Language -> Action -> IO ()
make' = make True []

execute :: Bool -> ProjectFunc -> ActionFunc -> IO ()
execute readProjectFile languageFunc actionFunc = do
    languageFunc (project >>= actionFunc) >>= \case
        (Left err) -> logger Error ("[mewlix] " <> Text.pack err)
        (Right _ ) -> return ()
    where project = if readProjectFile
            then readProject
            else return defaultProject
