{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Modules.StandardLibrary
( addLibraries
) where

import Mewlix.Project.Data.Types (ProjectMode(..))
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Project.Maker (Language(..))
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type Library = HashMap Key Text

library :: [(Text, Text)] -> Library
library = HashMap.mapKeys Key . HashMap.fromList

baseLibrary :: Language -> Library
baseLibrary JavaScript = library
    [ ("std"       , "Mewlix.Base"      )
    , ("std.curry" , "Mewlix.BaseCurry" ) ]

templateLibraries :: Language -> ProjectMode -> Library
templateLibraries JavaScript = \case
    Console -> library
        [ ("std.console"        , "Mewlix.Console"      )
        , ("std.console.curry"  , "Mewlix.ConsoleCurry" ) ]
    Graphic -> library
        [ ("std.graphic"        , "Mewlix.Graphic"      )
        , ("std.graphic.curry"  , "Mewlix.GraphicCurry" ) ]
    Library -> mempty

addLibraries :: Language -> ProjectMode -> (Library -> Library)
addLibraries language mode = mappend template . mappend base
    where base = baseLibrary language
          template = templateLibraries language mode
