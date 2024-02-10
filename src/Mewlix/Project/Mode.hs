{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Mode
( ProjectMode(..)
, modeKeys
, defaultMode
, readProjectMode
) where

import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , withText
    )
import Data.Text (Text)
import Data.Char (toLower)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)

data ProjectMode =
      Console
    | Graphic
    | Library
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance FromJSON ProjectMode where
    parseJSON = withText "ProjectMode" (return . readProjectMode)

instance ToJSON ProjectMode where
    toJSON = toJSON . map toLower . show

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
