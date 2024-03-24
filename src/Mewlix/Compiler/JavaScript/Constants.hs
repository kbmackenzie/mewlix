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
, yarnBall
-- Operations:
, meow
, listen
, wrap
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
, rainable
, pounceError
, assert
) where

{- This module should always be imported qualified. -}

import Prelude hiding (compare)
import Data.Text (Text)

mewlix :: Text -> Text
mewlix = ("Mewlix." <>)

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

yarnBall :: Text
yarnBall = mewlix "YarnBall"

{- Operations -}
--------------------------------------------
meow :: Text
meow = mewlix "meow"

listen :: Text
listen = mewlix "listen"

wrap :: Text
wrap = mewlix "wrap"

numbers :: Text -> Text
numbers = mewlix . ("Numbers." <>)

boolean :: Text -> Text
boolean = mewlix . ("Boolean." <>)

compare :: Text -> Text
compare = mewlix . ("Compare." <>)

strings :: Text -> Text
strings = mewlix . ("Strings." <>)

shelves :: Text -> Text
shelves = mewlix . ("Shelves." <>)

reflection :: Text -> Text
reflection = mewlix . ("Reflection." <>)

boxes :: Text -> Text
boxes = mewlix . ("Boxes." <>)

conversion :: Text -> Text
conversion = mewlix . ("Conversion." <>)

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
internal = mewlix . ("Internal." <>)

rainable :: Text
rainable = internal "rainable"

pounceError :: Text
pounceError = internal "pounceError"

assert :: Text
assert = internal "assert"
