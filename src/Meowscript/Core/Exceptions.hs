{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.Exceptions
( showException
, showException'
, stackTrace
, opException
, divByZero
, catOnComputer
, meowSyntaxExc
, notInLoop
, badKey
, notKey
, badDot
, badBox
, notBox
, manyArgs
, fewArgs
, badFunc
, badFuncDef
, notFunc
, badArgs
, badValue
, badTryCatch
, nestedImport
, badImport
, notImport
, badFile
, badHash
, noFile
, meowUnexpected
) where

import Meowscript.Core.AST
import Meowscript.Core.Pretty
import Meowscript.Utils.Types
import qualified Data.Text as Text
import qualified Data.List as List
import Control.Monad.Except (throwError, catchError)
import Data.Functor ((<&>))

showException :: MeowException -> Text.Text -> CatException
showException meowe message = (meowe,) $ Text.concat
    ["[", (Text.pack . show) meowe, "]", ": ", message, "\n  - Stack trace:" ]

showException' :: MeowException -> Text.Text -> [Prim] -> Evaluator CatException
showException' meowEx text xs = do
    prims <- mapM prettyMeow xs
    let terms = Text.intercalate ", " prims
    let message = [text, " | Terms: ", terms]
    (return . showException meowEx . Text.concat) message

stackTrace :: Evaluator Text.Text -> Evaluator a -> Evaluator a
stackTrace txt action = action `catchError` \(x, y) -> do
    message <- txt <&> ("\n    " `Text.append`)
    throwError (x, y `Text.append` message)

{- Exception Helpers -}
--------------------------------------------------------
opException :: Text.Text -> [Prim] -> Evaluator CatException
opException = showException' MeowInvalidOp . \x -> Text.concat
    [ "Invalid operands for '", x, "'." ]

divByZero :: [Prim] -> Evaluator CatException
divByZero = showException' MeowDivByZero "Cannot divide by zero!"

catOnComputer :: Text.Text -> CatException
catOnComputer = showException MeowCatOnComputer

meowSyntaxExc :: Text.Text -> CatException
meowSyntaxExc = showException MeowBadSyntax . Text.cons '\n'

badKey :: Text.Text -> CatException
badKey = showException MeowBadVar . \x -> Text.concat 
    ["Key '", x, "' doesn't exist in the current context!" ]

notKey :: [Prim] -> Evaluator CatException
notKey = showException' MeowNotKey "Token cannot be used as key: "

badDot :: Text.Text -> Prim -> Evaluator CatException
badDot key = showException' MeowBadVar message . List.singleton
    where message = Text.concat [ "Key '", key, "' doesn't exist in object!" ]

badBox :: Text.Text -> CatException
badBox = showException MeowBadBox . \x -> Text.concat ["Value in key '", x, "' is not a box!"]

notBox :: Prim -> Evaluator CatException
notBox prim = prettyMeow prim >>= \x -> return $ showException MeowBadBox
    $ Text.concat [ "Value '", x, "' is not a box!" ]

fewArgs :: Text.Text -> [Prim] -> Evaluator CatException
fewArgs x = showException' MeowBadArgs
    $ Text.concat [ "Not enough arguments passed to function '", x, "'!" ]

manyArgs :: Text.Text -> [Prim] -> Evaluator CatException
manyArgs x = showException' MeowBadArgs 
    $ Text.concat [ "Too many arguments passed to function '", x, "'!" ]

badFunc :: Text.Text -> CatException
badFunc = showException MeowBadFunc . \x -> Text.concat ["Key '", x, "' is not a function!"]

notFunc :: Prim -> Evaluator CatException
notFunc prim = prettyMeow prim >>= \x -> return $ showException MeowBadFunc
    $ Text.concat ["Attempted to call '", x, "' as a function," , " but '", x, "' is not a function!" ]

badFuncDef :: Prim -> Evaluator CatException
badFuncDef = showException' MeowBadArgs "Invalid function name: " . List.singleton

badArgs :: Text.Text -> [Prim] -> Evaluator CatException
badArgs = showException' MeowBadArgs . \x -> Text.concat
    [ "Invalid argument(s) passed to function '", x, "'!" ]

badValue :: Text.Text -> Text.Text -> [Prim] -> Evaluator CatException
badValue fn = showException' MeowBadValue . Text.append
    (Text.concat [ "In function '", fn, "': " ])

notInLoop :: Text.Text -> CatException
notInLoop = showException MeowNotKeyword . \x -> Text.concat
    [ "The '", x, "' keyword can only be used inside loops!" ]

badTryCatch :: Prim -> Evaluator CatException
badTryCatch = showException' MeowBadValue "Invalid value in 'catch' block." . List.singleton

nestedImport :: FilePathT -> CatException
nestedImport = showException MeowBadImport . \x -> Text.concat
    [ "Can't import module '", x,"' : "
    , "Import / 'takes as' statements cannot be nested!" ]

badImport :: FilePathT -> Text.Text -> CatException
badImport file exception = showException MeowBadImport
    (Text.concat ["Error when importing file '", file, "':\n", exception])

notImport :: Text.Text -> CatException
notImport = showException MeowUnexpected . Text.append "Token is not an import: "

badFile :: Text.Text -> FilePathT -> Text.Text -> CatException
badFile message path exception = showException MeowBadFile $ Text.concat
    [ message, ": | ", exception, " | File: \"", path, "\"" ]

badHash :: Prim -> Evaluator CatException
badHash = showException' MeowBadHash "Value cannot be hashed." . List.singleton

noFile :: FilePath -> CatException
noFile = showException MeowBadFile . Text.append "File not found in search paths: " . Text.pack

meowUnexpected :: Text.Text -> Text.Text -> CatException
meowUnexpected x y = showException MeowUnexpected (Text.concat [x, " | ", y])
