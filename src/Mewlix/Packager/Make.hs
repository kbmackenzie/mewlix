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
    , initProject
    , packageProject
    , runProject
    )
-- Assorted:
import Mewlix.Logger (LogLevel(..), logger);
import System.Exit (exitWith, ExitCode(..))

data Action =
      Build
    | Clean
    | Init
    | Package
    | Run
    deriving (Eq, Ord, Show, Enum, Bounded)

type ActionFunc  = ProjectConfig -> Packager ()

action :: Action -> ActionFunc
action Build    = buildProject
action Clean    = cleanProject
action Init     = initProject
action Package  = packageProject
action Run      = runProject

make :: Bool -> Maybe FilePath -> [ProjectTransform] -> Action -> IO ()
make shouldReadConfig configPath transforms chosenAction = execute
    shouldReadConfig
    configPath
    (action chosenAction . transformProject transforms)

make' :: Action -> IO ()
make' = make True Nothing []

execute :: Bool -> Maybe FilePath -> ActionFunc -> IO ()
execute shouldReadConfig configPath runAction = do
    packager (project >>= runAction) >>= \case
        (Left err) -> do
            logger LogError err
            exitWith (ExitFailure 1)
        (Right _ ) -> return ()
    where project = if shouldReadConfig
            then readProject configPath
            else return defaultProject
