{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Packager.Library
( getLibrary
) where

import Mewlix.Packager.Config (ProjectMode(..))
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

getTemplateLibrary :: ProjectMode -> Library
getTemplateLibrary = \case
    Console -> library
        [ "std.console"
        , "std.console.curry" ]
    Graphic -> library
        [ "std.graphic"
        , "std.graphic.curry" ]
    Node    -> mempty
    Blank   -> mempty

getLibrary :: ProjectMode -> Library
getLibrary = mappend baseLibrary . getTemplateLibrary
