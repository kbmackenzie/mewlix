{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.Exceptions
( stackTrace
-- Operations:
, opException
, divByZero
, catOnComputer
-- Syntax:
, meowSyntaxExc
-- Variables / Boxes:
, badKey
, notKey
, badDot
, badBox
, notBox
-- Functions:
, manyArgs
, fewArgs
, badFunc
, badFuncDef
, notFunc
, badArgs
, badValue
-- Statements:
, notInLoop
, badTryCatch
-- Imports:
, nestedImport
, badImport
, notImport
-- IO:
, badFile
, noFile
-- Hashing:
, badHash
-- Unexpected:
, meowUnexpected
) where

import Meowscript.Core.AST
import Meowscript.Core.Pretty
import Meowscript.Utils.Types
import qualified Data.Text as Text
import qualified Data.List as List
import Control.Monad.Except (throwError, catchError)
import Data.Functor ((<&>))

{- Utils -}
--------------------------------------------------------
makeException :: MeowException -> Text.Text -> CatException
makeException meowe message = (meowe,) $ Text.concat
    ["[", (Text.pack . show) meowe, "]", ": ", message, "\n  - Stack trace:" ]

makeException' :: MeowException -> Text.Text -> [Prim] -> Evaluator CatException
makeException' meowEx text xs = do
    prims <- mapM prettyMeow xs
    let terms = Text.intercalate ", " prims
    let message = [text, " | Terms: ", terms]
    (return . makeException meowEx . Text.concat) message

stackTrace :: Evaluator Text.Text -> Evaluator a -> Evaluator a
stackTrace txt action = action `catchError` \(x, y) -> do
    message <- txt <&> ("\n    " `Text.append`)
    throwError (x, y `Text.append` message)

{- Operations -}
--------------------------------------------------------
opException :: Text.Text -> [Prim] -> Evaluator CatException
opException = makeException' MeowInvalidOp . \x -> Text.concat
    [ "Invalid operands for '", x, "'." ]

divByZero :: [Prim] -> Evaluator CatException
divByZero = makeException' MeowDivByZero "Cannot divide by zero!"

catOnComputer :: Text.Text -> CatException
catOnComputer = makeException MeowCatOnComputer

{- Syntax -}
--------------------------------------------------------
meowSyntaxExc :: Text.Text -> CatException
meowSyntaxExc = makeException MeowBadSyntax . Text.cons '\n'

{- Variables / Boxes -}
--------------------------------------------------------
badKey :: Text.Text -> CatException
badKey = makeException MeowBadVar . \x -> Text.concat 
    ["Key '", x, "' doesn't exist in the current context!" ]

notKey :: [Prim] -> Evaluator CatException
notKey = makeException' MeowNotKey "Token cannot be used as key: "

badDot :: Text.Text -> Prim -> Evaluator CatException
badDot key = makeException' MeowBadVar message . List.singleton
    where message = Text.concat [ "Key '", key, "' doesn't exist in object!" ]

badBox :: Text.Text -> CatException
badBox = makeException MeowBadBox . \x -> Text.concat ["Value in key '", x, "' is not a box!"]

notBox :: Prim -> Evaluator CatException
notBox prim = prettyMeow prim >>= \x -> return $ makeException MeowBadBox
    $ Text.concat [ "Value '", x, "' is not a box!" ]

{- Functions -}
--------------------------------------------------------
fewArgs :: Text.Text -> [Prim] -> Evaluator CatException
fewArgs x = makeException' MeowBadArgs
    $ Text.concat [ "Not enough arguments passed to function '", x, "'!" ]

manyArgs :: Text.Text -> [Prim] -> Evaluator CatException
manyArgs x = makeException' MeowBadArgs 
    $ Text.concat [ "Too many arguments passed to function '", x, "'!" ]

badFunc :: Text.Text -> CatException
badFunc = makeException MeowBadFunc . \x -> Text.concat ["Key '", x, "' is not a function!"]

notFunc :: Prim -> Evaluator CatException
notFunc prim = prettyMeow prim >>= \x -> return $ makeException MeowBadFunc
    $ Text.concat ["Attempted to call '", x, "' as a function," , " but '", x, "' is not a function!" ]

badFuncDef :: Prim -> Evaluator CatException
badFuncDef = makeException' MeowBadArgs "Invalid function name: " . List.singleton

badArgs :: Text.Text -> [Prim] -> Evaluator CatException
badArgs = makeException' MeowBadArgs . \x -> Text.concat
    [ "Invalid argument(s) passed to function '", x, "'!" ]

badValue :: Text.Text -> Text.Text -> [Prim] -> Evaluator CatException
badValue fn = makeException' MeowBadValue . Text.append
    (Text.concat [ "In function '", fn, "': " ])

{- Statements -}
--------------------------------------------------------
notInLoop :: Text.Text -> CatException
notInLoop = makeException MeowNotKeyword . \x -> Text.concat
    [ "The '", x, "' keyword can only be used inside loops!" ]

badTryCatch :: Prim -> Evaluator CatException
badTryCatch = makeException' MeowBadValue "Invalid value in 'catch' block." . List.singleton

{- Imports -}
--------------------------------------------------------
nestedImport :: FilePathT -> CatException
nestedImport = makeException MeowBadImport . \x -> Text.concat
    [ "Can't import module '", x,"' : "
    , "Import / 'takes as' statements cannot be nested!" ]

badImport :: FilePathT -> Text.Text -> CatException
badImport file exception = makeException MeowBadImport
    (Text.concat ["Error when importing file '", file, "':\n", exception])

notImport :: Text.Text -> CatException
notImport = makeException MeowUnexpected . Text.append "Token is not an import: "

{- IO -}
--------------------------------------------------------
badFile :: Text.Text -> [FilePathT] -> Text.Text -> CatException
badFile message paths exception = makeException MeowBadFile $ Text.concat
    [ message, ": | ", exception, " | File: \"", Text.intercalate ", " paths, "\"" ]

noFile :: FilePath -> CatException
noFile = makeException MeowBadFile . Text.append "File not found in search paths: " . Text.pack

{- Hashing -}
--------------------------------------------------------
badHash :: Prim -> Evaluator CatException
badHash = makeException' MeowBadHash "Value cannot be hashed." . List.singleton

{- Unexpected -}
--------------------------------------------------------
meowUnexpected :: Text.Text -> Text.Text -> CatException
meowUnexpected x y = makeException MeowUnexpected (Text.concat [x, " | ", y])
