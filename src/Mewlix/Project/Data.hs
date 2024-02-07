module Mewlix.Project.Data
( ProjectMode(..)
, ProjectData(..)
) where

import Data.Text (Text)
import Data.HashSet (HashSet)

data ProjectMode =
      Console
    | Graphic
    | Library
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ProjectData = ProjectData
    { projectName           :: Text
    , projectDescription    :: Text
    , projectMode           :: ProjectMode
    , projectDirectories    :: HashSet FilePath }
    deriving (Show)
