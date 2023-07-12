{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Meowscript.Meowr.Core
( MeowrState(..)
, MeowrAction
, MeowrArg(..)
, ActionMap
, meowrArgs
, meowrSocket
, meowrDefines
, pushArgs
, addDefine
) where

import Meowscript.Core.AST
import Meowscript.Utils.IO
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Network.Socket (Socket)
import Lens.Micro.Platform

data MeowrState = MeowrState --todo
    { _meowrArgs   :: [Text.Text]
    , _meowrSocket :: Maybe Socket
    , _meowrDefines :: Map.Map Text.Text Text.Text }

$(makeLenses ''MeowrState)

type MeowrAction = MeowrArg -> MeowrState -> MeowrState
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

pushArgs :: Text.Text -> MeowrState -> MeowrState
pushArgs = over meowrArgs . (:)

addDefine :: Text.Text -> Text.Text -> MeowrState -> MeowrState
addDefine = (over meowrDefines .) . Map.insert
