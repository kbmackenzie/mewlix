{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Mewlix.Project.Data
( ProjectData(..)
-- Lenses:
, projectNameL
, projectDescriptionL
, projectModeL
, projectSourceFilesL
, projectSpecialImportsL
, projectFlagsL
-- Utils:
, projectDataEmpty
, projectFieldOrder
) where

import Mewlix.Project.Mode (ProjectMode(..), defaultMode)
import Data.Aeson
    ( ToJSON(..)
    , FromJSON(..)
    , withObject
    , (.:)
    , (.=)
    , (.:?)
    , object
    , pairs
    )
import Data.Text (Text)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Lens.Micro.Platform (makeLensesFor)
import Data.Function (on)

data ProjectData = ProjectData
    { projectName           :: Text
    , projectDescription    :: Text
    , projectMode           :: ProjectMode
    , projectSourceFiles    :: [FilePath]
    , projectSpecialImports :: HashMap Text Text
    , projectFlags          :: HashSet Text      }
    deriving (Show)

instance FromJSON ProjectData where
    parseJSON = withObject "ProjectData" $ \obj -> ProjectData
        <$> obj .: "name"
        <*> obj .: "description"
        <*> optional defaultMode (obj .:? "mode")
        <*> obj .: "sources"
        <*> optional mempty (obj .:? "specialImports")
        <*> optional mempty (obj .:? "flags"         )
        where
            optional :: (Functor f) => a -> f (Maybe a) -> f a
            optional = fmap . fromMaybe

instance ToJSON ProjectData where
    toJSON project = object
        [ "name"            .= projectName project
        , "description"     .= projectDescription project
        , "mode"            .= projectMode project
        , "sources"         .= projectSourceFiles project
        , "specialImports"  .= projectSpecialImports project
        , "flags"           .= projectFlags project ]

    toEncoding project = pairs $ mconcat
        [ "name"            .= projectName project
        , "description"     .= projectDescription project
        , "mode"            .= projectMode project
        , "sources"         .= projectSourceFiles project
        , "specialImports"  .= projectSpecialImports project
        , "flags"           .= projectFlags project ]

----------------------------------------------------------------

$(makeLensesFor
    [ ("projectName"            , "projectNameL"            )
    , ("projectDescription"     , "projectDescriptionL"     )
    , ("projectMode"            , "projectModeL"            )
    , ("projectSourceFiles"     , "projectSourceFilesL"     )
    , ("projectSpecialImports"  , "projectSpecialImportsL"  )
    , ("projectFlags"           , "projectFlagsL"           ) ] ''ProjectData)

----------------------------------------------------------------

projectDataEmpty :: ProjectData
projectDataEmpty = ProjectData mempty mempty Console mempty mempty mempty

projectFieldOrder :: Text -> Text -> Ordering
projectFieldOrder = compare `on` (`HashMap.lookup` fieldOrder)
    where 
        fieldOrder :: HashMap Text Int
        fieldOrder = HashMap.fromList
            [ ("name"           , 1)
            , ("description"    , 2)
            , ("mode"           , 3)
            , ("sources"        , 4)
            , ("specialImports" , 5)
            , ("flags"          , 6)
            ]
