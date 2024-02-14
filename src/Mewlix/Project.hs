module Mewlix.Project
( Language(..)
, Action(..)
, make
, makeSingle
, ProjectData(..)
, ProjectMode(..)
-- Utils:
, defaultMode
, defaultPort
, defaultName
, defaultEntry
) where

import Mewlix.Project.Data.Types
    ( ProjectData(..)
    , ProjectMode(..)
    , defaultMode
    , defaultPort
    , defaultName
    , defaultEntry
    )
import Mewlix.Project.Make (Action(..), Language(..), make, makeSingle)
