{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Parser.Keywords
( meowCatface
, meowLocal
, meowEnd
, meowReturn
, meowContinue
, meowBreak
, meowIf
, meowElse
, meowWhile
, meowFor
, meowPaw
, meowClaw
, meowBox
, meowLambda
, meowNot
, meowAnd
, meowOr
, meowPeek
, meowPush
, meowKnock
, meowTakes
, reservedKeywords
) where

import Data.Text (Text)

meowCatface :: Text
meowCatface = "=^.x.^="

meowLocal :: Text
meowLocal = "mew"

meowEnd :: Text
meowEnd = "meow meow"

meowReturn :: Text
meowReturn = "bring"

meowContinue :: Text
meowContinue = "catnap"

meowBreak :: Text
meowBreak = "run off"

meowIf :: Text
meowIf = "mew?"

meowElse :: Text
meowElse = "hiss!"

meowWhile :: Text
meowWhile = "meowmeow"

meowFor :: (Text, Text, Text)
meowFor = ("take", "and do", "while")

meowPaw :: Text
meowPaw = "paw at" 

meowClaw :: Text
meowClaw = "claw at"

meowBox :: Text
meowBox = "~( ^.x.^) BOX!!"

meowLambda :: Text
meowLambda = "( ^.x.^)>"

meowNot :: Text
meowNot = "not"

meowAnd :: Text
meowAnd = "and"

meowOr :: Text
meowOr = "or"

meowPush :: Text
meowPush = "push"

meowPeek :: Text
meowPeek = "peek"

meowKnock :: Text
meowKnock = "knock over"

meowTakes :: (Text, Text)
meowTakes = ("takes", "as")

reservedKeywords :: [Text]
{-# INLINE reservedKeywords #-}
reservedKeywords =
    [ "mew"
    , "paw"
    , "claw"
    , "mew?"
    , "hiss!"
    , "bring"
    , "catnap"
    , "run"
    , "off"
    , "and"
    , "or"
    , "at"
    , "do"
    , "take"
    , "takes"
    , "box"
    , "poke"
    , "push"
    , "peek"
    , "knock"
    , "over"
    , "happy"
    , "sad"
    , "lonely"
    , "meowmeow"]
