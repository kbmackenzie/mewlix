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
, meowBap
, meowPeek
, meowPush
, meowKnock
) where

import Data.Text (Text)

meowCatface :: Text
meowCatface = "=^.x.^="

meowEnd :: Text
meowEnd = "walk off"

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

meowBap :: Text
meowBap = "bap"

meowPush :: Text
meowPush = "push"

meowPeek :: Text
meowPeek = "peek"

meowKnock :: Text
meowKnock = "knock over"
