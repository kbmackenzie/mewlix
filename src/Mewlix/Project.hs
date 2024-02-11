module Mewlix.Project
( ProjectMaker
, Language(..)
, make
, singletonProject
) where

import Mewlix.Project.Make (ProjectMaker, Language(..))
import Mewlix.Project.Run (make, singletonProject)
