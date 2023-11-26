{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Keywords
( func
, local
, comment
, true
, false
, nil
, end
, ret
, catnap
, run
, clowder
, from
, home
, super
, constructor
, mewIf
, mewElif
, mewElse
, while
, takeDo
, paw
, claw
, box
, lambda
, mewNot
, mewAnd
, mewOr
, push
, peek
, pop
, takes
, mewTry
, mewCatch
, reserved
) where

import Data.Text (Text)
import Data.HashSet (HashSet, fromList)

{- This module should *always* be imported qualified.
 - It should be imported with the name 'Keywords' for consistency! -}

func :: Text
func = "=^.x.^="

local :: Text
local = "mew"

comment :: (Text, Text)
comment = ("~( ^.x.^)>", "<(^.x.^ )~")

true :: Text
true = "true"

false :: Text
false = "false"

nil :: Text
nil = "nothing"

end :: Text
end = "meow meow"

ret :: Text
ret = "bring"

catnap :: Text
catnap = "catnap"

run :: Text
run = "run off"

clowder :: Text
clowder = "clowder"

from :: Text
from = "from"

home :: Text
home = "home"

super :: Text
super = "parent"

constructor :: Text
constructor = "wake"

mewIf :: Text
mewIf = "mew?"

mewElif :: Text
mewElif = "mao!"

mewElse :: Text
mewElse = "hiss!"

while :: Text
while = "meowmeow"

takeDo :: (Text, Text, Text)
takeDo = ("take", "and do", "while")

paw :: Text
paw = "paw at" 

claw :: Text
claw = "claw at"

box :: Text
box = "=^-x-^="

lambda :: Text
lambda = "=^*x*^="

mewNot :: Text
mewNot = "not"

mewAnd :: Text
mewAnd = "and"

mewOr :: Text
mewOr = "or"

push :: Text
push = "push"

peek :: Text
peek = "peek"

pop :: Text
pop = "knock over"

takes :: (Text, Text)
takes = ("takes", "as")

mewTry :: Text
mewTry = "watch"

mewCatch :: Text
mewCatch = "catch"

reserved :: HashSet Text
{-# INLINE reserved #-}
reserved = fromList
    [ "mew"
    , "paw"
    , "claw"
    , "mew?"
    , "hiss!"
    , "mao"
    , "mao!"
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
    , "watch"
    , "catch"
    , "box"
    , "BOX"
    , "push"
    , "peek"
    , "knock"
    , "over"
    , "happy"
    , "sad"
    , "nothing"
    , "clowder"
    , "from"
    , "when"
    , "meowmeow" ]
