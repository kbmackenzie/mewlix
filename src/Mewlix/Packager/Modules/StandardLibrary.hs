{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Packager.Modules.StandardLibrary
( addLibraries
) where

import Mewlix.Packager.Data.Types (ProjectMode(..))
import Mewlix.Abstract.Key (Key(..))
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type Library = HashMap Key Text

library :: [(Text, Text)] -> Library
library = HashMap.mapKeys Key . HashMap.fromList

baseLibrary :: Library
baseLibrary = library
    [ ("std"       , "mewlix.Base"      )
    , ("std.curry" , "mewlix.BaseCurry" ) ]

templateLibraries :: ProjectMode -> Library
templateLibraries = \case
    Console -> library
        [ ("std.console"        , "mewlix.Console"      )
        , ("std.console.curry"  , "mewlix.ConsoleCurry" ) ]
    Graphic -> library
        [ ("std.graphic"        , "mewlix.Graphic"      )
        , ("std.graphic.curry"  , "mewlix.GraphicCurry" ) ]
    Node    -> mempty

addLibraries :: ProjectMode -> (Library -> Library)
addLibraries mode = mappend template . mappend baseLibrary
    where template = templateLibraries mode
