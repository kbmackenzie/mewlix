{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Exceptions
( MeowException(..)
, showException
, showException'
, opException
, divByZero
, catOnComputer
, badKey
, badBox
, notBox
, emptyTrail
, manyArgs
, fewArgs
, badFunc
, badTrail
, shortTrail
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
    | MeowCatOnComputer
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
    show MeowCatOnComputer = exc "CatOnComputer"

-- TO DO: 
-- Make these functions use a pretty-printing function instead of 'show'.

exc :: String -> String
exc = (++ "Exception")

showException :: MeowException -> Text.Text -> Text.Text
showException meowe message = Text.concat
    ["[", (Text.pack . show) meowe, "]", ": ", message]

showException' :: MeowException -> Text.Text -> [Prim] -> Evaluator Text.Text
showException' meowEx text xs = do
    prims <- mapM showMeow xs
    let terms = Text.intercalate ", " prims
    let message = [text, " | Terms: [", terms, "]"]
    (return . showException meowEx . Text.concat) message
    
opException :: Text.Text -> [Prim] -> Evaluator Text.Text
opException = showException' MeowInvalidOp . \x -> Text.concat
    [ "Invalid operands for '", x, "'!" ]

divByZero :: [Prim] -> Evaluator Text.Text
divByZero = showException' MeowDivByZero "Cannot divide by zero!"

catOnComputer :: Text.Text -> Text.Text
catOnComputer = showException MeowCatOnComputer

badKey :: Text.Text -> Text.Text
badKey = showException MeowBadVar . \x -> Text.concat 
    ["Key '", x, "' doesn't exist in the current context!" ]

badBox :: Text.Text -> Text.Text
badBox = showException MeowBadBox . \x -> Text.concat ["Value in key '", x, "' is not a box!"]

notBox :: Prim -> Evaluator Text.Text
notBox prim = showMeow prim >>= \x -> return $ showException MeowBadBox
    $ Text.concat [ "Value '", x, "' is not a box!" ]

emptyTrail :: Text.Text
emptyTrail = showException MeowBadTrail "Trail is empty!"

fewArgs :: Text.Text -> [Prim] -> Evaluator Text.Text
fewArgs x = showException' MeowBadArgs
    $ Text.concat [ "Too few arguments passed to function '", x, "'! | Terms:" ]

manyArgs :: Text.Text -> [Prim] -> Evaluator Text.Text
manyArgs x = showException' MeowBadArgs 
    $ Text.concat [ "Too many arguments passed to function '", x, "'!" ]

badFunc :: Text.Text -> Text.Text
badFunc = showException MeowBadFunc . \x -> Text.concat ["Key '", x, "' is not a function!"]

notFunc :: Prim -> Evaluator Text.Text
notFunc prim = showMeow prim >>= \x -> return $ showException MeowBadFunc
    $ Text.concat ["Attempted to call '", x, "' as a function," , " but '", x, "' is not a function!" ]

badTrail :: Text.Text -> Text.Text
badTrail = showException MeowBadTrail . Text.append "Invalid token in trail: "

shortTrail :: [Key] -> Text.Text
shortTrail = showException MeowBadTrail
    . Text.append "Trail is too short. | Trail: "
    . Text.intercalate "."

badArgs :: Text.Text -> [Prim] -> Evaluator Text.Text
badArgs = showException' MeowBadArgs . \x -> Text.concat
    [ "Invalid argument(s) passed to function '", x, "'!" ]

badValue :: Text.Text -> Text.Text -> [Prim] -> Evaluator Text.Text
badValue fn = showException' MeowBadValue . Text.append
    (Text.concat [ "In function: '", fn, "': " ])

nestedImport :: FilePath -> Text.Text
nestedImport = showException MeowBadImport . \x -> Text.concat
    [ "Can't import module '", Text.pack x,"' : "
    , "Import / 'takes as' statements cannot be nested!" ]

badImport :: FilePath -> Text.Text -> Text.Text
badImport file exception = showException MeowBadImport
    (Text.concat ["Error when importing file '", Text.pack file, "':\n", exception])
