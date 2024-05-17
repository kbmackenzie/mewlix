{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Packager.Make
( Action(..)
, make
, make'
) where

import Mewlix.Packager.Maker (PackageMaker(..), packageMake)
import Mewlix.Packager.Data.Types
    ( ProjectData(..)
    , ProjectTransform
    , transformProject
    , defaultProject
    )
-- Actions:
import Mewlix.Packager.Actions.Build (buildProject)
import Mewlix.Packager.Actions.Clean (cleanProject)
import Mewlix.Packager.Actions.Package (packageProject)
import Mewlix.Packager.Actions.Create (createProject)
import Mewlix.Packager.Actions.Run (runProject)
-- Assorted:
import Mewlix.Packager.Data.Read (readProject)
import Mewlix.Logger (LogType(..), logger);
import qualified Data.Text as Text

data Action =
      Build
    | Clean
    | Create
    | Package
    | Run
    deriving (Eq, Ord, Show, Enum, Bounded)

type ActionFunc  = ProjectData -> PackageMaker ()

action :: Action -> ActionFunc
action Build    = buildProject
action Clean    = cleanProject
action Create   = createProject
action Package  = packageProject
action Run      = runProject

make :: Bool -> [ProjectTransform] -> Action -> IO ()
make readProjectFile transforms actOption = execute
    readProjectFile
    (action actOption . transformProject transforms)

make' :: Action -> IO ()
make' = make True []

execute :: Bool -> ActionFunc -> IO ()
execute useProjectFile actionFunc = do
    packageMake (project >>= actionFunc) >>= \case
        (Left err) -> logger Error ("[mewlix] " <> Text.pack err)
        (Right _ ) -> return ()
    where project = if useProjectFile
            then readProject
            else return defaultProject
