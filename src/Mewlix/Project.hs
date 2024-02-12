module Mewlix.Project
( ProjectMaker
, Language(..)
, ProjectContext(..)
, ProjectData(..)
, ProjectMode(..)
, make
, singletonProject
) where

import Mewlix.Project.Maker (ProjectMaker, Language(..), ProjectContext(..))
import Mewlix.Project.Data.Types (ProjectData(..), ProjectMode(..))
import Mewlix.Project.Make (make, singletonProject)
