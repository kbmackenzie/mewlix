{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Mewlix.Keywords.Constants
( function
, local
, comment
, true
, false
, nil
, meow
, end
, ret
, catnap
, run
, clowder
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

import Mewlix.Keywords.Types
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Mewlix.Utils.Triple (fst3)

{- This module should *always* be imported qualified. -}

function :: LongSymbol
function = "=^.x.^="

local :: Keyword
local = "mew"

comment :: (LongSymbol, LongSymbol)
comment = ("~( ^.x.^)>", "<(^.x.^ )~")

true :: Keyword
true = "true"

false :: Keyword
false = "false"

nil :: Keyword
nil = "nothing"

meow :: Keyword
meow = "meow"

end :: Keyword
end = "meow meow"

ret :: Keyword
ret = "bring"

catnap :: Keyword
catnap = "catnap"

run :: Keyword
run = "run off"

clowder :: (Keyword, Keyword)
clowder = ("clowder", "is")

home :: Keyword
home = "home"

super :: Keyword
super = "parent"

constructor :: Keyword
constructor = "wake"

mewIf :: Keyword
mewIf = "mew?"

mewElif :: Keyword
mewElif = "mao!"

mewElse :: Keyword
mewElse = "hiss!"

while :: Keyword
while = "meowmeow"

takeDo :: (Keyword, WordSequence, Keyword)
takeDo = ("take", ["and", "do"], "while")

paw :: WordSequence
paw = ["paw", "at"]

claw :: WordSequence
claw = ["claw", "at"]

box :: LongSymbol
box = "=^-x-^="

lambda :: LongSymbol
lambda = "=^*x*^="

mewNot :: Keyword
mewNot = "not"

mewAnd :: Keyword
mewAnd = "and"

mewOr :: Keyword
mewOr = "or"

push :: Keyword
push = "push"

peek :: Keyword
peek = "peek"

pop :: WordSequence
pop = ["knock", "over"]

takes :: (Keyword, Keyword)
takes = ("takes", "as")

mewTry :: Keyword
mewTry = "watch"

mewCatch :: Keyword
mewCatch = "catch"

reserved :: HashSet Keyword
{-# INLINE reserved #-}
reserved = HashSet.fromList
    [ local
    , true
    , false
    , nil
    , meow
    , ret
    , catnap
    , run
    , fst clowder
    , snd clowder
    , fst3 takeDo
    , home
    , super
    , mewIf
    , mewElif
    , mewElse
    , while
    , (head . unwrapWords) paw
    , (head . unwrapWords) claw
    , mewNot
    , mewAnd
    , mewOr
    , push
    , peek
    , (head . unwrapWords) pop
    , fst takes
    , mewTry
    , mewCatch
    ]
