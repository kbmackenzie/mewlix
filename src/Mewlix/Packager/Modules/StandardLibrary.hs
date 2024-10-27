{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Packager.Modules.StandardLibrary
( addLibraries
) where

import Mewlix.Packager.Config.Types (ProjectMode(..))
import Mewlix.Abstract.Key (Key(..))
import Data.Text (Text)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

type Library = HashSet Key

library :: [Text] -> Library
library = HashSet.fromList . map Key

baseLibrary :: Library
baseLibrary = library
    [ "std"
    , "std.curry" ]

templateLibraries :: ProjectMode -> Library
templateLibraries = \case
    Console -> library
        [ "std.console"
        , "std.console.curry" ]
    Graphic -> library
        [ "std.graphic"
        , "std.graphic.curry" ]
    Node    -> mempty

addLibraries :: ProjectMode -> (Library -> Library)
addLibraries mode = mappend template . mappend baseLibrary
    where template = templateLibraries mode
