{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Mewlix.Keywords.LanguageKeywords
( function
, local
, pipe
, compose
, composeRef
, commentOpen
, commentClose
, true
, false
, nil
, meow
, end
, ret
, earlyRet
, catnap
, do_
, doArrow
, break
, clowder
, extends
, new
, home
, constructor
, superCall
, superRef
, catTree
, if_
, elif
, else_
, while
, forEach
, forEachOf
, typeOf
, is
, in_
, claw
, box
, lambda
, lambdaArrow
, not
, and
, or
, nand
, nor
, push
, peek
, pop
, ternIf
, ternElse
, takes
, alias
, from
, yarnball
, yarnball'
, try
, catch
, throw
, rethrow
, assert
, errorRef
, reserved
) where

import Mewlix.Keywords.Types
    ( SimpleKeyword(..)
    , LongSymbol(..)
    , WordSequence(..)
    , SymbolOptions(..)
    )
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Prelude hiding (and, or, not, break)

{- This module should *always* be imported qualified. -}

-- Functions:
function :: SymbolOptions
function = ["=^.x.^=", "🐱", "😺"]

-- Variables:
local :: SimpleKeyword
local = "mew"

-- Composition + Piping
compose :: LongSymbol
compose = ":>"

composeRef :: SimpleKeyword
composeRef = "___x"

pipe :: LongSymbol
pipe = "|>"

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

constructor :: SimpleKeyword
constructor = "wake"

superCall :: WordSequence
superCall = ["look", "outside"]

superRef :: SimpleKeyword
superRef = "___super"

-- Cat trees:
catTree :: WordSequence
catTree = ["cat", "tree"]

-- Expressions:
typeOf :: WordSequence
typeOf = ["type", "of"]

claw :: WordSequence
claw = ["claw", "at"]

is :: SimpleKeyword
is = "is"

in_ :: SimpleKeyword
in_ = "in"

box :: SymbolOptions
box = ["=^-x-^=", "📦"]

lambda :: SymbolOptions
lambda = ["=^oxo^=", "🐈"]

lambdaArrow :: LongSymbol
lambdaArrow = "->"

not :: SimpleKeyword
not = "not"

and :: SimpleKeyword
and = "and"

or :: SimpleKeyword
or = "or"

nand :: SimpleKeyword
nand = "nand"

nor :: SimpleKeyword
nor = "nor"

push :: SimpleKeyword
push = "push"

peek :: WordSequence
peek = ["paw", "at"]

pop :: WordSequence
pop = ["knock", "over"]

meow :: SimpleKeyword
meow = "meow"

do_ :: SimpleKeyword
do_ = "do"

doArrow :: LongSymbol
doArrow = "<-"

ternIf :: SimpleKeyword
ternIf = "if"

ternElse :: SimpleKeyword
ternElse = "else"

-- Statements:
end :: SimpleKeyword
end = "~meow"

ret :: SimpleKeyword
ret = "bring"

earlyRet :: WordSequence
earlyRet = ["run", "away"]

assert :: SimpleKeyword
assert = "assert"

catnap :: SimpleKeyword
catnap = "catnap"

break :: SimpleKeyword
break = "escape"

if_ :: WordSequence
if_ = ["look", "if"]

elif :: WordSequence
elif = ["or", "if"]

else_ :: WordSequence
else_ = ["else", "just"]

while :: WordSequence
while = ["stare", "while"]

forEach :: WordSequence
forEach = ["chase", "after"]

forEachOf :: SimpleKeyword
forEachOf = "in"

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

rethrow :: SimpleKeyword
rethrow = "rethrow"

errorRef :: SimpleKeyword
errorRef = "___error"

reserved :: HashSet SimpleKeyword
{-# INLINE reserved #-}
reserved = HashSet.fromList
    [ local
    , true
    , false
    , nil
    , meow
    , do_
    , composeRef
    , ternIf
    , ternElse
    , end
    , ret
    , catnap
    , break
    , clowder
    , extends
    , new
    , home
    , is
    , in_
    , not
    , and
    , or
    , nand
    , nor
    , push
    , forEachOf
    , yarnball'
    , takes
    , alias
    , from
    , throw
    , rethrow
    , superRef
    , errorRef
    -- Internal:
    , "mewlix"
    , "yarnball"
    , "globalThis"
    , "valueOf"
    , "toString"
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
    , "enum"
    , "implements"
    , "interface"
    , "package"
    , "private"
    , "protected"
    , "public"
    , "arguments"
    , "as"
    , "async"
    , "eval"
    , "from"
    ]
