{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Meowr.Project
( MeowrConfig(..)
, initConfig
, applyConfigs
, configState
) where

import Meowscript.Core.AST
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Meowscript.Core.Environment (createObject)
import Lens.Micro.Platform (makeLenses, view, over, set)
import Data.Bifunctor (second)
import Data.Maybe (mapMaybe)
import Data.IORef (newIORef, readIORef)
import Data.Foldable (foldrM)
import Control.Monad ((>=>))

{- Config Data -}
-------------------------------------------------
data MeowrConfig = MeowrConfig
    { _configMain   :: Text.Text
    , _configArgs   :: [Text.Text]
    , _configFlags  :: [Text.Text]
    , _sourceFiles  :: [Text.Text]
    , _projectInfo  :: [(Text.Text, Text.Text)] }
    deriving (Show)

$(makeLenses ''MeowrConfig)

{- Actions -}
-------------------------------------------------
type FieldName = Text.Text
type TransConfig a = a -> MeowrConfig -> MeowrConfig
type ConfigAction = ObjectMap -> MeowrConfig -> IO MeowrConfig

transField :: FieldName -> Extract a -> TransConfig a -> a -> ConfigAction
transField fieldName extractF transF defaultValue obj config = getField fieldName obj extractF >>= \case
    Nothing -> return (transF defaultValue config)
    (Just field) -> return (transF field config)

info :: Text.Text -> Text.Text -> MeowrConfig -> MeowrConfig
info key = over projectInfo . (:) . (key,)

configActions :: [ConfigAction]
configActions =
    [ transField "main"     extractStr      (set configMain)    "main.meows"
    , transField "args"     extractStrList  (set configArgs)    []          
    , transField "flags"    extractStrList  (set configFlags)   []
    , transField "sources"  extractStrList  (set sourceFiles)   []          
    , transField "name"     extractStr      (info "name")       Text.empty
    , transField "author"   extractStr      (info "author")     Text.empty
    , transField "license"  extractStr      (info "license")    Text.empty
    ]

{- State Transform -}
-------------------------------------------------
type StateConfig = MeowrConfig -> MeowState -> MeowState

transformStates :: [StateConfig]
transformStates =
    [ set meowPath . view configMain
    , set meowArgs . view configArgs
    , set meowInclude . map Text.unpack . view sourceFiles
    , over meowLib . nestLibs "__info__" . makeInfoObject . view projectInfo ]

configState :: MeowrConfig -> MeowState -> MeowState
configState config state = foldr ($) state transforms
    where transforms = map ($ config) transformStates

{- Creating Objects -}
-------------------------------------------------
makeInfoObject :: [(Text.Text, Text.Text)] -> IO PrimRef
makeInfoObject = (createObject >=> newIORef . MeowObject) . map (second MeowString)

{- Apply -}
-------------------------------------------------
applyConfigs :: MeowrConfig -> ObjectMap -> IO MeowrConfig
applyConfigs config obj = foldrM ($) config configs
    where configs = map ($ obj) configActions

initConfig :: MeowrConfig
initConfig = MeowrConfig
    { _configMain   = "main.meows"
    , _configArgs   = []
    , _configFlags  = []
    , _sourceFiles  = []
    , _projectInfo  = []            }

{- Extraction -}
-------------------------------------------------
type Extract a = Prim -> Maybe a

extractStr :: Extract Text.Text
extractStr (MeowString x) = Just x
extractStr _ = Nothing

extractStrList :: Extract [Text.Text]
extractStrList (MeowList xs) = Just $ mapMaybe extractStr xs
extractStrList _ = Nothing

type Extracted a = IO (Maybe a)

getField :: Text.Text -> ObjectMap -> Extract a -> Extracted a
getField key obj f = case Map.lookup key obj of
    Nothing -> return Nothing
    (Just a) -> f <$> readIORef a
