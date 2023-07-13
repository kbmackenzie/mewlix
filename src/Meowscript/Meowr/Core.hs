module Meowscript.Meowr.Core
( MeowrArg(..)
, MeowrAction
, ActionMap
, addArg
, addDefine
, isFlag
, isOption
) where

import Meowscript.Core.AST
import Meowscript.Utils.IO
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform (over)

type MeowrAction = MeowrArg -> MeowState -> MeowState
type ActionMap = Map.Map Text.Text MeowrAction

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

addArg :: Text.Text -> MeowState -> MeowState
addArg = over meowArgs . (:)

addDefine :: Text.Text -> Text.Text -> MeowState -> MeowState
addDefine = (over meowDefines .) . Map.insert
