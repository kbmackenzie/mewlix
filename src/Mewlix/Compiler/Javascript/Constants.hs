{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Javascript.Constants
( mewlix
, purrify
, mewlixBox
, stackNode
, stackBottom
, createStack
, meow
, listen
, operation
, addModule
, getModule
, mewlixClowder
, equalTo
, lessThan
, greaterThan
, watchPounce
, rainable
, assert
) where

{- This module should always be imported qualified. -}

import Data.Text (Text)

mewlix :: Text -> Text
mewlix = ("Mewlix." <>)

purrify :: Text
purrify = mewlix "purrify"

mewlixBox :: Text
mewlixBox = mewlix "MewlixBox"

stackNode :: Text
stackNode = mewlix "StackNode"

stackBottom :: Text
stackBottom = mewlix "StackBottom"

createStack :: Text
createStack = mewlix "MewlixStack.fromArray"

meow :: Text
meow = mewlix "meow"

listen :: Text
listen = mewlix "listen"

operation :: Text -> Text
operation = mewlix . ("Op." <>)

modules :: Text -> Text
modules = mewlix . ("Modules." <>)

addModule :: Text
addModule = modules "addModule"

getModule :: Text
getModule = modules "getModule"

mewlixClowder :: Text
mewlixClowder = mewlix "MewlixCloder"

comparison :: Text -> Text
comparison = mewlix . ("Comparison." <>)

equalTo :: Text
equalTo = comparison "EqualTo"

lessThan :: Text
lessThan = comparison "LessThan"

greaterThan :: Text
greaterThan = comparison "GreaterThan"

watchPounce :: Text
watchPounce = mewlix "watchPounce"

rainable :: Text
rainable = mewlix "rainable"

assert :: Text
assert = mewlix "assert"
