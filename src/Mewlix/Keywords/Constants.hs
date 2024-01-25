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
, new
, home
, super
, constructor
, if_
, elif
, else_
, while
, takeDo
, paw
, claw
, box
, lambda
, not
, and
, or
, push
, peek
, pop
, takes
, try
, catch
, throw
, reserved
) where

import Mewlix.Keywords.Types
    ( Keyword(..)
    , LongSymbol(..)
    , WordSequence(..)
    )
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Mewlix.Utils.Triple (fst3)
import Prelude hiding (and, or, not)

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

new :: Keyword
new = "meet"

home :: Keyword
home = "home"

super :: Keyword
super = "outside"

constructor :: Keyword
constructor = "wake"

if_ :: Keyword
if_ = "mew?"

elif :: Keyword
elif = "mao!"

else_ :: Keyword
else_ = "hiss!"

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

not :: Keyword
not = "not"

and :: Keyword
and = "and"

or :: Keyword
or = "or"

push :: Keyword
push = "push"

peek :: Keyword
peek = "peek"

pop :: WordSequence
pop = ["knock", "over"]

takes :: (Keyword, Keyword)
takes = ("takes", "as")

try :: Keyword
try = "watch"

catch :: Keyword
catch = "catch"

throw :: Keyword
throw = "throw"

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
    , if_
    , elif
    , else_
    , while
    , (head . unwrapWords) paw
    , (head . unwrapWords) claw
    , not
    , and
    , or
    , push
    , peek
    , (head . unwrapWords) pop
    , fst takes
    , try
    , catch
    -- Internal:
    , "box"
    , "___module_fn___"
    ]
