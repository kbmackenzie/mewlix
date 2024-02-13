{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Project.Data.Types
( ProjectMode(..)
, defaultMode
, ProjectData(..)
-- Lenses:
, projectNameL
, projectDescriptionL
, projectModeL
, projectEntrypointL
, projectSourceFilesL
, projectSpecialImportsL
, projectFlagsL
-- Utils:
, projectDataEmpty
, projectFieldOrder
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

{- Project Mode -}
----------------------------------------------------------------
data ProjectMode =
      Console
    | Graphic
    | Library
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

instance FromJSON ProjectMode where
    parseJSON = withText "ProjectMode" (return . readProjectMode)

instance ToJSON ProjectMode where
    toJSON = toJSON . map toLower . show

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

defaultMode :: ProjectMode
defaultMode = Console

readProjectMode :: Text -> ProjectMode
readProjectMode = fromMaybe defaultMode . findKey . prepText
    where prepText = Text.toLower . Text.strip
          findKey  = flip HashMap.lookup modeKeys

{- Project Data -}
----------------------------------------------------------------
data ProjectData = ProjectData
    { projectName           :: Text
    , projectDescription    :: Text
    , projectMode           :: ProjectMode
    , projectEntrypoint     :: Text
    , projectSourceFiles    :: [FilePath]
    , projectSpecialImports :: HashMap Text Text
    , projectFlags          :: HashSet Text      }
    deriving (Show)

instance FromJSON ProjectData where
    parseJSON = withObject "ProjectData" $ \obj -> ProjectData
        <$> optional "no-name"      (obj .:  "name"          )
        <*> optional mempty         (obj .:  "description"   )
        <*> optional defaultMode    (obj .:? "mode"          )
        <*> optional "main"         (obj .:? "entrypoint"    )
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
        , "sources"         .= projectSourceFiles project
        , "specialImports"  .= projectSpecialImports project
        , "flags"           .= projectFlags project ]

    toEncoding project = pairs $ mconcat
        [ "name"            .= projectName project
        , "description"     .= projectDescription project
        , "mode"            .= projectMode project
        , "entrypoint"      .= projectEntrypoint project
        , "sources"         .= projectSourceFiles project
        , "specialImports"  .= projectSpecialImports project
        , "flags"           .= projectFlags project ]

----------------------------------------------------------------

$(makeLensesFor
    [ ("projectName"            , "projectNameL"            )
    , ("projectDescription"     , "projectDescriptionL"     )
    , ("projectMode"            , "projectModeL"            )
    , ("projectEntrypoint"      , "projectEntrypointL"      )
    , ("projectSourceFiles"     , "projectSourceFilesL"     )
    , ("projectSpecialImports"  , "projectSpecialImportsL"  )
    , ("projectFlags"           , "projectFlagsL"           ) ] ''ProjectData)

----------------------------------------------------------------

{- Project Utils -}
----------------------------------------------------------------
projectDataEmpty :: ProjectData
projectDataEmpty = ProjectData
    { projectName           = "no-name"
    , projectDescription    = mempty
    , projectMode           = defaultMode
    , projectEntrypoint     = "main"
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
            , ("sources"        , 5)
            , ("specialImports" , 6)
            , ("flags"          , 7) ]
