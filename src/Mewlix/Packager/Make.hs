{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Packager.Make
( Action(..)
, make
, make'
) where

import Mewlix.Packager.Type (Packager(..), packager)
import Mewlix.Packager.Config
    ( ProjectConfig(..)
    , ProjectTransform
    , transformProject
    , defaultProject
    , readProject
    )
-- Actions:
import Mewlix.Packager.Actions
    ( buildProject
    , cleanProject
    , packageProject
    , createProject
    , runProject
    )
-- Assorted:
import Mewlix.Logger (LogData(..), LogType(..), logger);
import qualified Data.Text as Text

data Action =
      Build
    | Clean
    | Create
    | Package
    | Run
    deriving (Eq, Ord, Show, Enum, Bounded)

type ActionFunc  = ProjectConfig -> Packager ()

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
    packager (project >>= actionFunc) >>= \case
        (Left err) -> logger LogData
            { logType    = LogError
            , logPrefix  = Just "[mewlix: packager error]"
            , logMessage = Text.pack err }
        (Right _ ) -> return ()
    where project = if useProjectFile
            then readProject
            else return defaultProject
