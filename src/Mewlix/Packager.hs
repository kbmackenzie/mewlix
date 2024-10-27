module Mewlix.Packager
( Action(..)
, make
, make'
, ProjectData(..)
, ProjectFlag(..)
, ProjectMode(..)
, Port(..)
-- Compiler utils:
, addLibraries
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
, projectAssetsL
, projectFlagsL
) where

import Mewlix.Packager.Make (Action(..), make, make')
import Mewlix.Packager.Config
    ( ProjectData(..)
    , ProjectMode(..)
    , ProjectFlag(..)
    , Port(..)
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
    , projectAssetsL
    , projectFlagsL
    )
import Mewlix.Packager.Modules (addLibraries)
