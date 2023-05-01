{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Parser.Keywords
( meowCatface
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
, meowNot
, meowPeek
, meowPush
, meowKnock
, meowTakes
) where

import Data.Text (Text)

meowCatface :: Text
meowCatface = "=^.x.^="

meowEnd :: Text
meowEnd = "meow meow"

meowReturn :: Text
meowReturn = "bring"

meowContinue :: Text
meowContinue = "rest"

meowBreak :: Text
meowBreak = "run away"

meowIf :: Text
meowIf = "mew?"

meowElse :: Text
meowElse = "hiss!"

meowWhile :: Text
meowWhile = "scratch while"

meowFor :: (Text, Text, Text)
meowFor = ("steal", "and do", "while")

meowPaw :: Text
meowPaw = "paw at" 

meowClaw :: Text
meowClaw = "claw at"

meowNot :: Text
meowNot = "poke"

meowPush :: Text
meowPush = "push"

meowPeek :: Text
meowPeek = "peek"

meowKnock :: Text
meowKnock = "knock over"

meowTakes :: (Text, Text)
meowTakes = ("takes", "as")
