{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.Constants
( mewlix
, purrify
-- Data:
, box
, shelfNode
, shelfBottom
, createShelf
, clowder
, wake
, catTree
, yarnBall
, defaultKey
-- Operations:
, meow
, numbers
, boolean
, compare
, strings
, shelves
, reflection
, boxes
, conversion
-- Modules:
, addModule
, getModule
-- Comparison:
, equalTo
, lessThan
, greaterThan
-- Statement utils:
, canChase
, pounceError
, assert
) where

{- This module should always be imported qualified. -}

import Prelude hiding (compare)
import Data.Text (Text)

mewlix :: Text -> Text
mewlix = ("mewlix." <>)

purrify :: Text
purrify = mewlix "purrify"

{- Data -}
--------------------------------------------
box :: Text
box = mewlix "Box"

shelfNode :: Text
shelfNode = mewlix "ShelfNode"

shelfBottom :: Text
shelfBottom = mewlix "ShelfBottom"

createShelf :: Text
createShelf = mewlix "Shelf.fromArray"

clowder :: Text
clowder = mewlix "Clowder"

wake :: Text
wake = mewlix "wake"

catTree :: Text
catTree = mewlix "CatTree"

yarnBall :: Text
yarnBall = mewlix "YarnBall"

defaultKey :: Text
defaultKey = "main"

{- Operations -}
--------------------------------------------
meow :: Text
meow = mewlix "meow"

numbers :: Text -> Text
numbers = mewlix . ("numbers." <>)

boolean :: Text -> Text
boolean = mewlix . ("boolean." <>)

compare :: Text -> Text
compare = mewlix . ("compare." <>)

strings :: Text -> Text
strings = mewlix . ("strings." <>)

shelves :: Text -> Text
shelves = mewlix . ("shelves." <>)

reflection :: Text -> Text
reflection = mewlix . ("reflection." <>)

boxes :: Text -> Text
boxes = mewlix . ("boxes." <>)

conversion :: Text -> Text
conversion = mewlix . ("conversion." <>)

{- Modules: -}
--------------------------------------------
modules :: Text -> Text
modules = mewlix . ("Modules." <>)

addModule :: Text
addModule = modules "addModule"

getModule :: Text
getModule = modules "getModule"

{- Comparisons: -}
--------------------------------------------
comparison :: Text -> Text
comparison = mewlix . ("Comparison." <>)

equalTo :: Text
equalTo = comparison "EqualTo"

lessThan :: Text
lessThan = comparison "LessThan"

greaterThan :: Text
greaterThan = comparison "GreaterThan"

{- Statement Utils: -}
--------------------------------------------
internal :: Text -> Text
internal = mewlix . ("internal." <>)

canChase :: Text
canChase = internal "canChase"

pounceError :: Text
pounceError = internal "pounceError"

assert :: Text
assert = internal "assert"
