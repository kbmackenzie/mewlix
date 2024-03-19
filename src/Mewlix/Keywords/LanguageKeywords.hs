{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Mewlix.Keywords.LanguageKeywords
( function
, local
, commentOpen
, commentClose
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
, extends
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
, is
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
, from
, yarnball
, yarnball'
, try
, catch
, throw
, assert
, reserved
) where

import Mewlix.Keywords.Types
    ( SimpleKeyword(..)
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
local :: SimpleKeyword
local = "mew"

-- Block Comments:
commentOpen :: LongSymbol
commentOpen = "~( ^.x.^)>"

commentClose :: LongSymbol
commentClose = "<(^.x.^ )~"

-- Primitives:
true :: SimpleKeyword
true = "true"

false :: SimpleKeyword
false = "false"

nil :: SimpleKeyword
nil = "nothing"

-- Clowders:
clowder :: SimpleKeyword
clowder = "clowder"

extends :: SimpleKeyword
extends = "is"

new :: SimpleKeyword
new = "new"

home :: SimpleKeyword
home = "home"

super :: SimpleKeyword
super = "outside"

constructor :: SimpleKeyword
constructor = "wake"

-- Expressions:
paw :: WordSequence
paw = ["paw", "at"]

claw :: WordSequence
claw = ["claw", "at"]

is :: SimpleKeyword
is = "is"

box :: LongSymbol
box = "=^-x-^="

lambda :: LongSymbol
lambda = "=^*x*^="

not :: SimpleKeyword
not = "not"

and :: SimpleKeyword
and = "and"

or :: SimpleKeyword
or = "or"

push :: SimpleKeyword
push = "push"

peek :: SimpleKeyword
peek = "peek"

pop :: WordSequence
pop = ["knock", "over"]

meow :: SimpleKeyword
meow = "meow"

listen :: SimpleKeyword
listen = "listen"

-- Statements:
end :: SimpleKeyword
end = "meowmeow"

ret :: SimpleKeyword
ret = "bring"

assert :: SimpleKeyword
assert = "assert"

catnap :: SimpleKeyword
catnap = "catnap"

break :: SimpleKeyword
break = "escape"

if_ :: WordSequence
if_ = ["peek", "if"]

elif :: WordSequence
elif = ["or", "if"]

else_ :: SimpleKeyword
else_ = "otherwise"

while :: WordSequence
while = ["stare", "while"]

forEach :: WordSequence
forEach = ["it's", "raining"]

thenDo :: WordSequence
thenDo = ["catch", "a"]

takes :: SimpleKeyword
takes = "takes"

alias :: SimpleKeyword
alias = "as"

from :: SimpleKeyword
from = "from"

yarnball :: WordSequence
yarnball = ["yarn", "ball"]

yarnball' :: SimpleKeyword
yarnball' = "yarnball"

try :: WordSequence
try = ["watch", "attentively"]

catch :: WordSequence
catch = ["pounce", "on"]

throw :: SimpleKeyword
throw = "throw"

reserved :: HashSet SimpleKeyword
{-# INLINE reserved #-}
reserved = HashSet.fromList
    [ local
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
    , extends
    , new
    , home
    , super
    , firstWord paw
    , is
    , firstWord claw
    , not
    , and
    , or
    , push
    , peek
    , else_
    , firstWord yarnball
    , yarnball'
    , firstWord pop
    , takes
    , alias
    , from
    , throw
    -- Internal:
    , "box"
    , "valueOf"
    , "toString"
    , "globalThis"
    , "window"
    -- JavaScript:
    , "break"
    , "case"
    , "catch"
    , "class"
    , "const"
    , "continue"
    , "debugger"
    , "default"
    , "delete"
    , "do"
    , "else"
    , "export"
    , "extends"
    , "false"
    , "finally"
    , "for"
    , "function"
    , "if"
    , "import"
    , "in"
    , "instanceof"
    , "new"
    , "null"
    , "return"
    , "super"
    , "switch"
    , "this"
    , "throw"
    , "true"
    , "try"
    , "typeof"
    , "var"
    , "void"
    , "while"
    , "with"
    , "let"
    , "static"
    , "yield"
    , "await"
    ]
