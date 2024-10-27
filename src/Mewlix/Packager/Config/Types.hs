{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Packager.Config.Types
( ProjectMode(..)
, ProjectFlag(..)
, ProjectData(..)
, Port(..)
-- Lenses:
, projectNameL
, projectDescriptionL
, projectModeL
, projectEntrypointL
, projectPortL
, projectSourceFilesL
, projectAssetsL
, projectFlagsL
-- Project Utils:
, defaultProject
, ProjectTransform
, transformProject
, projectFieldOrder
-- Defaults:
, defaultMode
, defaultPort
, defaultName
, defaultEntry
) where

import Data.Aeson
    ( ToJSON(..)
    , FromJSON(..)
    , withObject
    , (.=)
    , (.:?)
    , object
    , withText
    , Value(..)
    )
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Lens.Micro.Platform (makeLensesFor)
import Data.Aeson.Types (unexpected)
import Data.Function (on)
import Data.Char (toLower)

{- Project Mode -}
----------------------------------------------------------------
data ProjectMode =
      Console
    | Graphic
    | Node
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- Project Flags -}
----------------------------------------------------------------
data ProjectFlag =
      Quiet
    | Pretty
    | Rebuild
    | NoStd
    | NoReadMe
    | NoBrowser
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- Project Data -}
----------------------------------------------------------------
data ProjectData = ProjectData
    { projectName           :: Text
    , projectDescription    :: Text
    , projectMode           :: ProjectMode
    , projectEntrypoint     :: Text
    , projectPort           :: Port
    , projectSourceFiles    :: [FilePath]
    , projectAssets         :: [FilePath]
    , projectFlags          :: Set ProjectFlag  }
    deriving (Show)


{- Port -}
----------------------------------------------------------------
newtype Port = Port { getPort :: Int }
    deriving (Eq, Ord, Show)

{- Lenses -}
----------------------------------------------------------------
$(makeLensesFor
    [ ("projectName"            , "projectNameL"            )
    , ("projectDescription"     , "projectDescriptionL"     )
    , ("projectMode"            , "projectModeL"            )
    , ("projectEntrypoint"      , "projectEntrypointL"      )
    , ("projectPort"            , "projectPortL"            )
    , ("projectSourceFiles"     , "projectSourceFilesL"     )
    , ("projectAssets"          , "projectAssetsL"          )
    , ("projectFlags"           , "projectFlagsL"           ) ] ''ProjectData)

{- Defaults -}
----------------------------------------------------------------
defaultMode :: ProjectMode
defaultMode = Graphic

defaultPort :: Port
defaultPort = Port 8143

defaultName :: Text
defaultName = "no-name"

defaultEntry :: Text
defaultEntry = "main"

{- Instances -}
-----------------------------------------------------------------
instance FromJSON ProjectMode where
    parseJSON = withText "ProjectMode" readMode

instance ToJSON ProjectMode where
    toJSON = toJSON . map toLower . show

------------------------------------------------------------------
instance FromJSON ProjectFlag where
    parseJSON = withText "ProjectFlag" readFlag

instance ToJSON ProjectFlag where
    toJSON = toJSON . flagToString

----------------------------------------------------------------
instance FromJSON ProjectData where
    parseJSON = withObject "ProjectData" $ \obj -> ProjectData
        <$> optional defaultName    (obj .:? "name"        )
        <*> optional mempty         (obj .:? "description" )
        <*> optional defaultMode    (obj .:? "mode"        )
        <*> optional defaultEntry   (obj .:? "entrypoint"  )
        <*> optional defaultPort    (obj .:? "port"        )
        <*> optional mempty         (obj .:? "source-files")
        <*> optional mempty         (obj .:? "assets"      )
        <*> optional mempty         (obj .:? "flags"       )
        where
            optional :: (Functor f) => a -> f (Maybe a) -> f a
            optional = fmap . fromMaybe

instance ToJSON ProjectData where
    toJSON project = object
        [ "name"            .= projectName project
        , "description"     .= projectDescription project
        , "mode"            .= projectMode project
        , "entrypoint"      .= projectEntrypoint project
        , "port"            .= projectPort project
        , "source-files"    .= projectSourceFiles project
        , "assets"          .= projectAssets project
        , "flags"           .= projectFlags project         ]

------------------------------------------------------------------
instance ToJSON Port where
    toJSON port
        | port == defaultPort = toJSON ("auto" :: String)
        | otherwise           = toJSON (getPort port)

instance FromJSON Port where
    parseJSON value = case value of
        (Number _)      -> Port <$> parseJSON value
        (String "auto") -> return defaultPort
        _               -> unexpected value

{- Flag Utils -}
----------------------------------------------------------------
parseFlag :: Text -> Maybe ProjectFlag
parseFlag key = case parse key of
    "quiet"      -> return Quiet
    "pretty"     -> return Pretty
    "rebuild"    -> return Rebuild
    "no-std"     -> return NoStd
    "no-readme"  -> return NoReadMe
    "no-browser" -> return NoBrowser
    _            -> Nothing
    where parse = Text.dropWhile (== '-')

readFlag :: (MonadFail m) => Text -> m ProjectFlag
readFlag str = case parse str of
    (Just flag) -> return flag
    Nothing     -> fail $ mconcat
        [ "Couldn't parse ", show str, " as a valid project flag!" ]
    where parse = parseFlag . Text.toLower . Text.strip

flagToString :: ProjectFlag -> String
flagToString flag = case flag of
    NoStd     -> "no-std"
    NoReadMe  -> "no-readme"
    NoBrowser -> "no-browser"
    _         -> (map toLower . show) flag

{- Mode Utils -}
----------------------------------------------------------------
parseMode :: Text -> Maybe ProjectMode
parseMode key = case key of
    "console" -> return Console
    "graphic" -> return Graphic
    "node"    -> return Node
    _         -> Nothing

readMode :: (MonadFail m) => Text -> m ProjectMode
readMode str = case parse str of
    (Just mode) -> return mode
    Nothing     -> fail $ mconcat
        [ "Couldn't parse ", show str, " as a valid value for ProjectMode!" ]
    where parse = parseMode . Text.toLower . Text.strip

{- Project Utils -}
----------------------------------------------------------------
defaultProject :: ProjectData
defaultProject = ProjectData
    { projectName           = defaultName
    , projectDescription    = mempty
    , projectMode           = defaultMode
    , projectEntrypoint     = defaultEntry
    , projectPort           = defaultPort
    , projectSourceFiles    = mempty
    , projectAssets         = mempty
    , projectFlags          = mempty       }

type ProjectTransform = ProjectData -> ProjectData

transformProject :: [ProjectTransform] -> ProjectData -> ProjectData
transformProject = flip (foldr ($))

fieldOrder :: HashMap Text Int
fieldOrder = HashMap.fromList
    [ ("name"         , 1)
    , ("description"  , 2)
    , ("mode"         , 3)
    , ("entrypoint"   , 4)
    , ("port"         , 5)
    , ("source-files" , 6)
    , ("assets"       , 7)
    , ("flags"        , 8) ]

projectFieldOrder :: Text -> Text -> Ordering
projectFieldOrder = compare `on` (`HashMap.lookup` fieldOrder)
