module Mewlix.Project
( Language(..)
, Action(..)
, make
, makeSingle
, ProjectData(..)
, ProjectMode(..)
-- Utils:
, defaultMode
) where

import Mewlix.Project.Data.Types (ProjectData(..), ProjectMode(..), defaultMode)
import Mewlix.Project.Make (Action(..), Language(..), make, makeSingle)
