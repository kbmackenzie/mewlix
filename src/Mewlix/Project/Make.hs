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
    )
-- Actions:
import Mewlix.Project.Actions.Build (buildProject)
import Mewlix.Project.Actions.Clean (cleanProject)
import Mewlix.Project.Actions.Package (packageProject)
import Mewlix.Project.Actions.Server (runProject)
-- Assorted:
import Mewlix.Project.Data.Read (readProject)
import qualified Mewlix.Utils.Logging as Logging
import qualified Data.Text as Text

data Action =
      Build
    | Clean
    | Package
    | Run
    deriving (Eq, Ord, Show, Enum, Bounded)

type ActionFunc  = ProjectData -> ProjectMaker ()
type ProjectFunc = ProjectMaker () -> IO (Either String ())

language :: Language -> ProjectFunc
language Javascript = projectMakeJS

action :: Action -> ActionFunc
action Build    = buildProject
action Clean    = cleanProject
action Package  = packageProject
action Run      = runProject

make :: [ProjectTransform] -> Language -> Action -> IO ()
make transforms langOption actOption = execute
    (language langOption)
    (action actOption . transformProject transforms)

make' :: Language -> Action -> IO ()
make' = make []

execute :: ProjectFunc -> ActionFunc -> IO ()
execute languageFunc actionFunc = do
    languageFunc (readProject >>= actionFunc) >>= \case
        (Left err) -> Logging.writeStderr (Text.pack err)
        (Right _ ) -> return ()
