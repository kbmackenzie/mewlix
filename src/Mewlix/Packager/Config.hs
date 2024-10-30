module Mewlix.Packager.Config
( ProjectMode(..)
, ProjectFlag(..)
, ProjectConfig(..)
, Port(..)
-- Lenses:
, projectNameL
, projectDescriptionL
, projectModeL
, projectEntrypointL
, projectPortL
, projectSourceFilesL
, projectAssetsL
, projectFlagsL
-- Project Utils:
, defaultProject
, ProjectTransform
, transformProject
, projectFieldOrder
-- Defaults:
, defaultMode
, defaultPort
, defaultName
, defaultEntry
-- Reading:
, readProject
) where

import Mewlix.Packager.Config.Types
    ( ProjectMode(..)
    , ProjectFlag(..)
    , ProjectConfig(..)
    , Port(..)
    -- Lenses:
    , projectNameL
    , projectDescriptionL
    , projectModeL
    , projectEntrypointL
    , projectPortL
    , projectSourceFilesL
    , projectAssetsL
    , projectFlagsL
    -- Project Utils:
    , defaultProject
    , ProjectTransform
    , transformProject
    , projectFieldOrder
    -- Defaults:
    , defaultMode
    , defaultPort
    , defaultName
    , defaultEntry
    )
import Mewlix.Packager.Config.Read (readProject)
