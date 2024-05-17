module Mewlix.Packager
( Action(..)
, make
, make'
, ProjectData(..)
, ProjectFlag(..)
, ProjectMode(..)
, Port(..)
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
, projectSpecialImportsL
, projectFlagsL
) where

import Mewlix.Packager.Make (Action(..), make, make')
import Mewlix.Packager.Data.Types
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
    , projectSpecialImportsL
    , projectFlagsL
    )
