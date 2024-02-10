{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Data
( ProjectData(..)
) where

import Mewlix.Project.Mode (ProjectMode, defaultMode)
import Data.Aeson
    ( ToJSON(..)
    , FromJSON(..)
    , withObject
    , (.:)
    , (.=)
    , (.:?)
    , object
    )
import Data.Text (Text)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)

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
