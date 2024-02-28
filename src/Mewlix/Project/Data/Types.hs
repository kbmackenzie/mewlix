{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Project.Data.Types
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
, projectSpecialImportsL
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
    , (.:)
    , (.=)
    , (.:?)
    , object
    , pairs
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
    | Library
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- Project Flags -}
----------------------------------------------------------------
data ProjectFlag =
      Quiet
    | NoStd
    | NoReadMe
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
    , projectSpecialImports :: HashMap Text Text
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
    , ("projectSpecialImports"  , "projectSpecialImportsL"  )
    , ("projectFlags"           , "projectFlagsL"           ) ] ''ProjectData)

{- Defaults -}
----------------------------------------------------------------
defaultMode :: ProjectMode
defaultMode = Console

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
    toJSON = toJSON . map toLower . show

----------------------------------------------------------------
instance FromJSON ProjectData where
    parseJSON = withObject "ProjectData" $ \obj -> ProjectData
        <$> optional defaultName    (obj .:? "name"          )
        <*> optional mempty         (obj .:? "description"   )
        <*> optional defaultMode    (obj .:? "mode"          )
        <*> optional defaultEntry   (obj .:? "entrypoint"    )
        <*> optional defaultPort    (obj .:? "port"          )
        <*> optional mempty         (obj .:  "sources"       )
        <*> optional mempty         (obj .:? "specialImports")
        <*> optional mempty         (obj .:? "flags"         )
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
        , "sources"         .= projectSourceFiles project
        , "specialImports"  .= projectSpecialImports project
        , "flags"           .= projectFlags project ]

    toEncoding project = pairs $ mconcat
        [ "name"            .= projectName project
        , "description"     .= projectDescription project
        , "mode"            .= projectMode project
        , "entrypoint"      .= projectEntrypoint project
        , "port"            .= projectPort project
        , "sources"         .= projectSourceFiles project
        , "specialImports"  .= projectSpecialImports project
        , "flags"           .= projectFlags project ]

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
flagKeys :: HashMap Text ProjectFlag
flagKeys = HashMap.fromList
    -- Names:
    [ ("quiet"     , Quiet     )
    , ("no-std"    , NoStd     )
    , ("no-readme" , NoReadMe  )
    -- Shorthand:
    , ("q"         , Quiet     ) ]

readFlag :: (MonadFail m) => Text -> m ProjectFlag
readFlag str = case parse str of
    (Just flag) -> return flag
    Nothing     -> fail $ mconcat
        [ "Couldn't parse " , show str , " as a valid project flag!" ]
    where parse = (`HashMap.lookup` flagKeys) . Text.toLower . Text.strip

{- Mode Utils -}
----------------------------------------------------------------
modeKeys :: HashMap Text ProjectMode
modeKeys = HashMap.fromList
    -- Names:
    [ ("console" , Console)
    , ("graphic" , Graphic) 
    , ("library" , Library)
    -- Shorthand:
    , ("c"       , Console)
    , ("g"       , Graphic)
    , ("l"       , Library) ]

readMode :: (MonadFail m) => Text -> m ProjectMode
readMode str = case parse str of
    (Just mode) -> return mode
    Nothing     -> fail $ mconcat
        [ "Couldn't parse ", show str, " as a valid value for ProjectMode!" ]
    where parse = (`HashMap.lookup` modeKeys) . Text.toLower . Text.strip

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
    , projectSpecialImports = mempty
    , projectFlags          = mempty            }

type ProjectTransform = ProjectData -> ProjectData

transformProject :: [ProjectTransform] -> ProjectData -> ProjectData
transformProject = flip (foldr ($))

projectFieldOrder :: Text -> Text -> Ordering
projectFieldOrder = compare `on` (`HashMap.lookup` fieldOrder)
    where 
        fieldOrder :: HashMap Text Int
        fieldOrder = HashMap.fromList
            [ ("name"           , 1)
            , ("description"    , 2)
            , ("mode"           , 3)
            , ("entrypoint"     , 4)
            , ("port"           , 5)
            , ("sources"        , 6)
            , ("specialImports" , 7)
            , ("flags"          , 8) ]
