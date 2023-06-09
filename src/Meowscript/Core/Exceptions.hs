{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Exceptions
( MeowException(..)
, showException
, showException'
, stackTrace
, opException
, divByZero
, catOnComputer
, notInLoop
, badKey
, badBox
, notBox
, emptyTrail
, manyArgs
, fewArgs
, badFunc
, badFuncDef
, badTrail
, shortTrail
, notFunc
, badArgs
, badValue
, nestedImport
, badImport
, badFile
, meowUnexpected
) where

import Meowscript.Core.AST
import Meowscript.Core.Pretty
import qualified Data.Text as Text
import qualified Data.List as List
import Control.Monad.Except (throwError, catchError)
import Data.Functor ((<&>))

data MeowException =
      MeowBadVar
    | MeowInvalidOp
    | MeowStackOverflow
    | MeowBadBox
    | MeowDivByZero
    | MeowBadTrail
    | MeowBadArgs
    | MeowBadToken
    | MeowBadFunc
    | MeowBadImport
    | MeowBadIFunc
    | MeowBadValue
    | MeowCatOnComputer
    | MeowBadKeyword
    | MeowBadFile
    | MeowBadFuncDef
    | MeowUnexpected
    deriving (Eq)

instance Show MeowException where
    show MeowBadVar = exc "InvalidVariable"
    show MeowInvalidOp = exc "InvalidOperation"
    show MeowStackOverflow = exc "StackOverflow"
    show MeowBadBox = exc "InvalidBox"
    show MeowDivByZero = exc "DivisionByZero"
    show MeowBadTrail = exc "InvalidTrail"
    show MeowBadArgs = exc "Argument"
    show MeowBadToken = exc "InvalidToken"
    show MeowBadFunc = exc "InvalidFunction"
    show MeowBadImport = exc "InvalidImport"
    show MeowBadIFunc = exc "InvalidInnerFunction"
    show MeowBadValue = exc "InvalidValue"
    show MeowCatOnComputer = exc "CatOnComputer"
    show MeowBadKeyword = exc "KeywordException"
    show MeowBadFile = exc "File"
    show MeowBadFuncDef = exc "FunctionDefinition"
    show MeowUnexpected = exc "Unexpected" 

exc :: String -> String
exc = (++ "Exception")

showException :: MeowException -> Text.Text -> Text.Text
showException meowe message = Text.concat
    ["[", (Text.pack . show) meowe, "]", ": ", message, "\n  - Stack trace:" ]

showException' :: MeowException -> Text.Text -> [Prim] -> Evaluator Text.Text
showException' meowEx text xs = do
    prims <- mapM prettyMeow xs
    let terms = Text.intercalate ", " prims
    let message = [text, " | Terms: [", terms, "]"]
    (return . showException meowEx . Text.concat) message

stackTrace :: Evaluator Text.Text -> Evaluator a -> Evaluator a
stackTrace txt action = action `catchError` \x -> do
    message <- txt <&> ("\n    " `Text.append`)
    throwError (x `Text.append` message)

--meowExcs :: [MeowException]
--meowExcs = [minBound..maxBound]

opException :: Text.Text -> [Prim] -> Evaluator Text.Text
opException = showException' MeowInvalidOp . \x -> Text.concat
    [ "Invalid operands for '", x, "'!" ]

divByZero :: [Prim] -> Evaluator Text.Text
divByZero = showException' MeowDivByZero "Cannot divide by zero!"

catOnComputer :: Text.Text -> Text.Text
catOnComputer = showException MeowCatOnComputer

notInLoop :: Text.Text -> Text.Text
notInLoop = showException MeowBadKeyword . \x -> Text.concat
    [ "The '", x, "' keyword can only be used inside loops!" ]

badKey :: Text.Text -> Text.Text
badKey = showException MeowBadVar . \x -> Text.concat 
    ["Key '", x, "' doesn't exist in the current context!" ]

badBox :: Text.Text -> Text.Text
badBox = showException MeowBadBox . \x -> Text.concat ["Value in key '", x, "' is not a box!"]

notBox :: Prim -> Evaluator Text.Text
notBox prim = prettyMeow prim >>= \x -> return $ showException MeowBadBox
    $ Text.concat [ "Value '", x, "' is not a box!" ]

emptyTrail :: Text.Text
emptyTrail = showException MeowBadTrail "Trail is empty!"

fewArgs :: Text.Text -> [Prim] -> Evaluator Text.Text
fewArgs x = showException' MeowBadArgs
    $ Text.concat [ "Not enough arguments passed to function '", x, "'!" ]

manyArgs :: Text.Text -> [Prim] -> Evaluator Text.Text
manyArgs x = showException' MeowBadArgs 
    $ Text.concat [ "Too many arguments passed to function '", x, "'!" ]

badFunc :: Text.Text -> Text.Text
badFunc = showException MeowBadFunc . \x -> Text.concat ["Key '", x, "' is not a function!"]

notFunc :: Prim -> Evaluator Text.Text
notFunc prim = prettyMeow prim >>= \x -> return $ showException MeowBadFunc
    $ Text.concat ["Attempted to call '", x, "' as a function," , " but '", x, "' is not a function!" ]

badFuncDef :: Prim -> Evaluator Text.Text
badFuncDef = showException' MeowBadArgs "Invalid function name: " . List.singleton

badTrail :: [Prim] -> Evaluator Text.Text
badTrail = showException' MeowBadTrail "Invalid token in trail: "

shortTrail :: [Key] -> Text.Text
shortTrail = showException MeowBadTrail
    . Text.append "Trail is too short. | Trail: "
    . Text.intercalate "."

badArgs :: Text.Text -> [Prim] -> Evaluator Text.Text
badArgs = showException' MeowBadArgs . \x -> Text.concat
    [ "Invalid argument(s) passed to function '", x, "'!" ]

badValue :: Text.Text -> Text.Text -> [Prim] -> Evaluator Text.Text
badValue fn = showException' MeowBadValue . Text.append
    (Text.concat [ "In function '", fn, "': " ])

nestedImport :: FilePath -> Text.Text
nestedImport = showException MeowBadImport . \x -> Text.concat
    [ "Can't import module '", Text.pack x,"' : "
    , "Import / 'takes as' statements cannot be nested!" ]

badImport :: FilePath -> Text.Text -> Text.Text
badImport file exception = showException MeowBadImport
    (Text.concat ["Error when importing file '", Text.pack file, "':\n", exception])

badFile :: Text.Text -> Text.Text -> Text.Text -> Text.Text
badFile path fn message = showException MeowBadFile $ Text.concat
    [ "In '", fn, "': ", message, " | File: \"", path, "\"" ]

meowUnexpected :: Text.Text -> Text.Text -> Text.Text
meowUnexpected x y = showException MeowUnexpected (Text.concat [x, " | ", y])
