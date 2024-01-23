{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Constants
( mewlix
, mewlixBox
, stackNode
, stackBottom
, createStack
, operation
, addModule
, getModule
) where

{- This module should always be imported qualified. -}

import Data.Text (Text)
import Mewlix.String.Utils ((|++))

mewlix :: Text -> Text
mewlix = ("Mewlix." |++)

mewlixBox :: Text
mewlixBox = mewlix "MewlixBox"

stackNode :: Text
stackNode = mewlix "StackNode"

stackBottom :: Text
stackBottom = mewlix "StackBottom"

createStack :: Text
createStack = mewlix "MewlixStack.fromArray"

operation :: Text -> Text
operation = mewlix . ("Op." |++)

modules :: Text -> Text
modules = mewlix . ("Modules." |++)

addModule :: Text
addModule = modules "addModule"

getModule :: Text
getModule = modules "getModule"
