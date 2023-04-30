{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Parser.Keywords
( meowCatface
, meowLeave
, meowReturn
, meowContinue
, meowBreak
, meowIf
, meowElse
, meowWhile
, meowBap
, meowPoke
, meowPeek
, meowNudge
, meowSneak
) where

import Data.Text (Text)

meowCatface :: Text
meowCatface = "=^.x.^="

meowLeave :: Text
meowLeave = "leave"

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

meowPoke :: Text
meowPoke = "poke"

meowPeek :: Text
meowPeek = "peek"

meowNudge :: Text
meowNudge = "nudge"

meowSneak :: Text
meowSneak = "sneak"
