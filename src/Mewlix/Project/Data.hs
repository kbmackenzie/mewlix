{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Data
( ProjectData(..)
) where

import Mewlix.Project.Mode (ProjectMode, readProjectMode)
import Data.Text (Text)
import Data.Aeson
    ( ToJSON(..)
    , FromJSON(..)
    , withObject
    , (.:)
    , (.=)
    , object
    )
import qualified Data.Text as Text
import Mewlix.Utils.Show (showT)

data ProjectData = ProjectData
    { projectName           :: Text
    , projectDescription    :: Text
    , projectMode           :: ProjectMode
    , projectSourceFiles    :: [FilePath]        }
    deriving (Show)

instance FromJSON ProjectData where
    parseJSON = withObject "ProjectData" $ \obj -> ProjectData
        <$> obj .: "name"
        <*> obj .: "description"
        <*> fmap readProjectMode (obj .: "mode")
        <*> obj .: "sources"

instance ToJSON ProjectData where
    toJSON project = object
        [ "name"        .= projectName project
        , "description" .= projectDescription project
        , "mode"        .= (Text.toLower . showT . projectMode) project
        , "sources"     .= projectSourceFiles project ]
