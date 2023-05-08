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
, notFunc
, badArgs
, badValue
, nestedImport
, badImport
) where

import Meowscript.Core.AST
import Meowscript.Core.Pretty
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
    | MeowBadIFunc
    | MeowBadValue
    deriving (Eq, Ord)

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

-- TO DO: 
-- Make these functions use a pretty-printing function instead of 'show'.

exc :: String -> String
exc = (++ "Exception")

showException :: MeowException -> Text.Text -> Text.Text
showException meowe message = Text.concat
    ["[", (Text.pack . show) meowe, "]", ": ", message]

showException' :: (Show a) => MeowException -> Text.Text -> [a] -> Text.Text
showException' meowe text xs = (showException meowe . Text.concat) message
    where terms = Text.intercalate ", " (map showT xs)
          message = [text, " | Terms: [", terms, "]"]

opException :: Text.Text -> [Prim] -> Text.Text
opException = showException' MeowInvalidOp . \x -> Text.concat
    [ "Invalid operands for '", x, "'!" ]

divByZero :: [Prim] -> Text.Text
divByZero = showException' MeowDivByZero "Cannot divide by zero!"

badKey :: Text.Text -> Text.Text
badKey = showException MeowBadVar . \x -> Text.concat 
    ["Key '", x, "' doesn't exist in the current context!" ]

badBox :: Text.Text -> Text.Text
badBox = showException MeowBadBox . \x -> Text.concat ["Value in key '", x, "' is not a box!"]

emptyTrail :: Text.Text
emptyTrail = showException MeowBadTrail "Trail is empty!"

fewArgs :: Text.Text -> Text.Text
fewArgs = showException MeowBadArgs . \x -> Text.concat
    [ "Too few arguments passed to function '", x, "'!" ]

manyArgs :: Text.Text -> Text.Text
manyArgs = showException MeowBadArgs . \x -> Text.concat
    [ "Too many arguments passed to function '", x, "'!" ]

badFunc :: Text.Text -> Text.Text
badFunc = showException MeowBadFunc . \x -> Text.concat ["Key '", x, "' is not a function!"]

notFunc :: Prim -> Text.Text
notFunc = showException MeowBadFunc . \x -> let prim = showT x in Text.concat
    [ "Attempted to call '", prim, "' as a function,"
    , " but '", prim, "' is not a function!" ]

badTrail :: Text.Text -> Text.Text
badTrail = showException MeowBadTrail . Text.append "Invalid token in trail: "

badArgs :: Text.Text -> [Prim] -> Text.Text
badArgs = showException' MeowBadArgs . \x -> Text.concat
    [ "Invalid argument(s) passed to function '", x, "'!" ]

badValue :: Text.Text -> Text.Text -> [Prim] -> Text.Text
badValue fn = showException' MeowBadValue . Text.append
    (Text.concat [ "In function: '", fn, "': " ])

nestedImport :: FilePath -> Text.Text
nestedImport = showException MeowBadImport . \x -> Text.concat
    [ "Can't import module '", Text.pack x,"' : "
    , "Import / 'takes as' statements cannot be nested!" ]

badImport :: FilePath -> Text.Text -> Text.Text
badImport file exception = showException MeowBadImport
    (Text.concat ["Error when importing file '", Text.pack file, "':\n", exception])
