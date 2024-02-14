module Mewlix.Project
( Language(..)
, Action(..)
, ProjectData(..)
, ProjectMode(..)
, Port
, make
, makeSingle
-- Utils:
, defaultMode
, defaultPort
, defaultName
, defaultEntry
) where

import Mewlix.Project.Data.Types
    ( ProjectData(..)
    , ProjectMode(..)
    , Port
    , defaultMode
    , defaultPort
    , defaultName
    , defaultEntry
    )
import Mewlix.Project.Make (Language(..), Action(..), make, makeSingle)
