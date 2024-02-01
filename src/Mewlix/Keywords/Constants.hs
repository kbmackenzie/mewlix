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
, listen
, end
, ret
, catnap
, break
, clowder
, new
, home
, super
, constructor
, if_
, elif
, else_
, while
, forEach
, thenDo
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
, alias
, try
, catch
, throw
, reserved
) where

import Mewlix.Keywords.Types
    ( Keyword(..)
    , LongSymbol(..)
    , WordSequence(..)
    , firstWord
    )
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Prelude hiding (and, or, not, break)

{- This module should *always* be imported qualified. -}

-- Functions:
function :: LongSymbol
function = "=^.x.^="

-- Variables:
local :: Keyword
local = "mew"

-- Block Comments:
comment :: (LongSymbol, LongSymbol)
comment = ("~( ^.x.^)>", "<(^.x.^ )~")

-- Primitives:
true :: Keyword
true = "true"

false :: Keyword
false = "false"

nil :: Keyword
nil = "nothing"

-- Clowders:
clowder :: (Keyword, Keyword)
clowder = ("clowder", "is")

new :: Keyword
new = "new"

home :: Keyword
home = "home"

super :: Keyword
super = "outside"

constructor :: Keyword
constructor = "wake"

-- Expressions:
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

meow :: Keyword
meow = "meow"

listen :: Keyword
listen = "listen"

-- Statements:
end :: Keyword
end = "meowmeow"

ret :: Keyword
ret = "bring"

catnap :: Keyword
catnap = "catnap"

break :: Keyword
break = "escape"

if_ :: WordSequence
if_ = ["peek", "and", "see", "if"]

elif :: WordSequence
elif = ["or", "if", "maybe"]

else_ :: WordSequence
else_ = ["otherwise", "just"]

while :: WordSequence
while = ["stare", "until"]

forEach :: WordSequence
forEach = ["it's", "raining"]

thenDo :: WordSequence
thenDo = ["catch", "a"]

takes :: Keyword
takes = "takes"

alias :: Keyword
alias = "as"

try :: WordSequence
try = ["watch", "attentively"]

catch :: WordSequence
catch = ["pounce", "on"]

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
    , listen
    , ret
    , catnap
    , break
    , fst clowder
    , snd clowder
    , new
    , home
    , super
    , firstWord paw
    , firstWord claw
    , not
    , and
    , or
    , push
    , peek
    , firstWord pop
    , takes
    , throw
    -- Internal:
    , "box"
    , "___module_fn___"
    ]
