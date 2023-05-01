{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Exceptions
( MeowException(..)
, showException
, showException'
, opException
, divByZero
, badKey
, badBox
, emptyTrail
, manyArgs
, fewArgs
, badFunc
, badTrail
, badImport
) where

import Meowscript.Core.AST
import qualified Data.Text as Text

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
    deriving (Eq, Ord)

instance Show MeowException where
    show MeowBadVar = "BadVariableException"
    show MeowInvalidOp = "InvalidOperationException"
    show MeowStackOverflow = "StackOverflowException"
    show MeowBadBox = "InvalidBoxException"
    show MeowDivByZero = "DivisionByZeroException"
    show MeowBadTrail = "InvalidTrailException"
    show MeowBadArgs = "ArgumentException"
    show MeowBadToken = "InvalidTokenException"
    show MeowBadFunc = "InvalidFunctionException"
    show MeowBadImport = "InvalidImportException"

showException :: MeowException -> Text.Text -> Text.Text
showException meowe message = Text.concat [(Text.pack . show) meowe, ": ", message]

showException' :: (Show a) => MeowException -> Text.Text -> [a] -> Text.Text
showException' meowe text xs = (showException meowe . Text.concat) message
    where terms = Text.intercalate ", " (map showT xs)
          message = [text, " | Terms: [", terms, "]"]

opException :: Text.Text -> [Prim] -> Text.Text
opException = showException' MeowInvalidOp

divByZero :: [Prim] -> Text.Text
divByZero = showException' MeowDivByZero "Cannot divide by zero!"

badKey :: Text.Text -> Text.Text
badKey = showException MeowBadVar . Text.append "Key doesn't exist in the current context: "

badBox :: Text.Text -> Text.Text
badBox = showException MeowBadBox . \x -> Text.concat ["Value in key '", x, "' is not a box!"]

emptyTrail :: Text.Text
emptyTrail = showException MeowBadTrail "Trail is empty!"

fewArgs :: Text.Text -> Text.Text
fewArgs = showException MeowBadArgs . Text.append "Too few arguments in function call: "

manyArgs :: Text.Text -> Text.Text
manyArgs = showException MeowBadArgs . Text.append "Too many arguments in function call: "

badFunc :: Text.Text -> Text.Text
badFunc = showException MeowBadFunc . \x -> Text.concat ["Key '", x, "' is not a function!"]

badTrail :: Text.Text -> Text.Text
badTrail = showException MeowBadTrail . Text.append "Invalid token in trail: "

badImport :: Text.Text -> Text.Text
badImport = showException MeowBadImport . \x -> Text.concat
    [ "Can't import module '", x,"' : "
    , "Import / 'take as' statements cannot be nested!" ]
