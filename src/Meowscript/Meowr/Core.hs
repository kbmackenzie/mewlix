{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Meowr.Core
( MeowrAction(..)
, MeowrArg(..)
, addArg
, addFlag
, addOption
, isFlag
, isOption
, isMeowrStr
, getMeowrStr
) where

import Meowscript.Core.AST
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro.Platform (over)
import Meowscript.Utils.Map (expandAsMap)

data MeowrAction = MeowrAction
    { meowrName :: Maybe Text.Text
    , meowrArgs :: [MeowrArg] }
    deriving (Show)

data MeowrArg =
      MeowrString Text.Text
    | MeowrFlag Text.Text
    | MeowrOption Text.Text Text.Text
    deriving (Show)

type TransOption = Text.Text -> MeowState -> MeowState

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
addArg = over meowArgsL . (:)

addFlag :: Text.Text -> MeowState -> MeowState
addFlag = over meowFlagsL . Set.insert . Text.toLower

addOption :: Text.Text -> Text.Text -> MeowState -> MeowState
addOption key value = case Map.lookup key meowrOptions of
    Nothing -> addDefine key value
    (Just f) -> f value

{- Options -}
--------------------------------------------------------------
meowrOptions :: Map.Map Text.Text TransOption
meowrOptions = expandAsMap
    [ ( meowrDefine  ,  emptyDef    )
    , ( meowrInclude ,  addInclude  ) ]

meowrDefine :: [Text.Text]
meowrDefine = ["d", "define", "def"]

meowrInclude :: [Text.Text]
meowrInclude = ["l", "library", "lib"]

addDefine :: Text.Text -> Text.Text -> MeowState -> MeowState
addDefine = (over meowDefinesL .) . Map.insert

emptyDef :: Text.Text -> MeowState -> MeowState
emptyDef def = addDefine def Text.empty

addInclude :: Text.Text -> MeowState -> MeowState
addInclude = over meowIncludeL . (:) . Text.unpack

{- Utils -}
--------------------------------------------------------------
getMeowrStr :: MeowrArg -> Text.Text
getMeowrStr (MeowrString x) = x
getMeowrStr _ = undefined -- This should never happen.
