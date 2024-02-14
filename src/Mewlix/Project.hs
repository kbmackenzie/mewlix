module Mewlix.Project
( Language(..)
, Action(..)
, ProjectData(..)
, ProjectMode(..)
, Port
, make
, makeSingle
-- Project utils:
, ProjectTransform
, transformProject
-- Defaults:
, defaultMode
, defaultPort
, defaultName
, defaultEntry
-- Lenses:
, projectNameL
, projectDescriptionL
, projectModeL
, projectEntrypointL
, projectPortL
, projectSourceFilesL
, projectSpecialImportsL
, projectFlagsL
) where

import Mewlix.Project.Data.Types
    ( ProjectData(..)
    , ProjectMode(..)
    , Port
    , defaultMode
    , defaultPort
    , defaultName
    , defaultEntry
    , ProjectTransform
    , transformProject
    -- Lenses:
    , projectNameL
    , projectDescriptionL
    , projectModeL
    , projectEntrypointL
    , projectPortL
    , projectSourceFilesL
    , projectSpecialImportsL
    , projectFlagsL
    )
import Mewlix.Project.Make (Language(..), Action(..), make, makeSingle)
