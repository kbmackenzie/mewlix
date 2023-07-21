module Meowscript.Meowr.Core
( MeowrAction(..)
, MeowrArg(..)
, addArg
, addFlag
, addDefine
, isFlag
, isOption
, isMeowrStr
) where

import Meowscript.Core.AST
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro.Platform (over)

data MeowrAction = MeowrAction
    { meowrName :: Maybe Text.Text
    , meowrArgs :: [MeowrArg] }
    deriving (Show)

data MeowrArg =
      MeowrString Text.Text
    | MeowrFlag Text.Text
    | MeowrOption Text.Text Text.Text
    deriving (Show)

isFlag :: MeowrArg -> Bool 
isFlag (MeowrFlag _) = True
isFlag _ = False

isOption :: MeowrArg -> Bool
isOption (MeowrOption _ _) = True
isOption _ = False

isMeowrStr :: MeowrArg -> Bool
isMeowrStr (MeowrString _) = True
isMeowrStr _ = False

addArg :: Text.Text -> MeowState -> MeowState
addArg = over meowArgs . (:)

addFlag :: Text.Text -> MeowState -> MeowState
addFlag = over meowFlags . Set.insert . Text.toLower

addDefine :: Text.Text -> Text.Text -> MeowState -> MeowState
addDefine = (over meowDefines .) . Map.insert
