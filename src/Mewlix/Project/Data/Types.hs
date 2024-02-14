{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Project.Data.Types
( ProjectMode(..)
, ProjectData(..)
, Port
-- Lenses:
, projectNameL
, projectDescriptionL
, projectModeL
, projectEntrypointL
, projectPortL
, projectSourceFilesL
, projectSpecialImportsL
, projectFlagsL
-- Utils:
, projectDataEmpty
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
    )
import Data.HashSet (HashSet)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Lens.Micro.Platform (makeLensesFor)
import Data.Function (on)
import Data.Char (toLower)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Network.Wai.Handler.Warp (Port)

{- Project Mode -}
----------------------------------------------------------------
data ProjectMode =
      Console
    | Graphic
    | Library
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

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
    , projectFlags          :: HashSet Text      }
    deriving (Show)

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
defaultPort = 8000

defaultName :: Text
defaultName = "no-name"

defaultEntry :: Text
defaultEntry = "main"

{- Instances -}
-----------------------------------------------------------------
instance FromJSON ProjectMode where
    parseJSON = withText "ProjectMode" (return . readProjectMode)

instance ToJSON ProjectMode where
    toJSON = toJSON . map toLower . show

-----------------------------------------------------------------
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

-----------------------------------------------------------------

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

readProjectMode :: Text -> ProjectMode
readProjectMode = fromMaybe defaultMode . findKey . prepText
    where prepText = Text.toLower . Text.strip
          findKey  = flip HashMap.lookup modeKeys

{- Project Utils -}
----------------------------------------------------------------
projectDataEmpty :: ProjectData
projectDataEmpty = ProjectData
    { projectName           = defaultName
    , projectDescription    = mempty
    , projectMode           = defaultMode
    , projectEntrypoint     = defaultEntry
    , projectPort           = defaultPort
    , projectSourceFiles    = mempty
    , projectSpecialImports = mempty
    , projectFlags          = mempty            }

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
