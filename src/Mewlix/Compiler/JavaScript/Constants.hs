{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.Constants
( mewlix
, purrify
-- Data:
, numbers
, strings
, boolean
, shelf
, box
, clowder
, catTree
, collections
, yarnball
-- Core Functions
, modules
, internal
-- Core Operations
, compare
, relation
, reflection
, convert
) where

{- This module should always be imported qualified. -}

import Prelude hiding (compare)
import Data.Text (Text)

mewlix :: Text -> Text
mewlix = mappend "mewlix."

purrify :: Text
purrify = mewlix "purrify"

-- Data 
--------------------------------------------
numbers :: Text -> Text
numbers = mewlix . mappend "numbers."

strings :: Text -> Text
strings = mewlix . mappend "strings."

boolean :: Text -> Text
boolean = mewlix . mappend "boolean."

shelf :: Text -> Text
shelf = mewlix . mappend "shelf."

box :: Text -> Text
box = mewlix . mappend "box."

clowder :: Text -> Text
clowder = mewlix . mappend "clowder."

catTree :: Text -> Text
catTree = mewlix . mappend "catTree."

collections :: Text -> Text
collections = mewlix . mappend "collections."

yarnball :: Text -> Text
yarnball = mewlix . mappend "yarnball."

-- Core Functions
--------------------------------------------
modules :: Text -> Text
modules = mewlix . mappend "modules."

internal :: Text -> Text
internal = mewlix . mappend "internal."

-- Core Operations
--------------------------------------------
compare :: Text -> Text
compare = mewlix . mappend "compare."

relation :: Text -> Text
relation = mewlix . mappend "relation."

reflection :: Text -> Text
reflection = mewlix . mappend "reflection."

convert :: Text -> Text
convert = mewlix . mappend "convert."
