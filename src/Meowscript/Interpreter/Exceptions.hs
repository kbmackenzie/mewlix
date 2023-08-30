{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Interpreter.Exceptions
( exceptionBase
, operationException
, divisionByZeroException
, unboundException
, notABoxException
, notAPropertyException
, notAnIdentifier
) where

import Meowscript.Abstract.Atom
import Meowscript.Abstract.Prettify
import Meowscript.Evaluate.Exception
import Control.Monad.IO.Class (MonadIO(..))
import Meowscript.Utils.Show
import qualified Data.Text as Text

exceptionBase :: (MonadIO m) => MeowException -> Text.Text -> [MeowAtom] -> m CatException
exceptionBase meow text xs = do
    items <- mapM prettyMeow xs
    let meowPretty = Text.concat [ "[", showT meow, "]" ]
    let message = Text.concat [ "[", meowPretty, "] ", text, " | Terms: ", Text.intercalate ", " items ]
    return CatException { exceptionType = meow, exceptionMessage = message }

operationException :: (MonadIO m) => Text.Text -> [MeowAtom] -> m CatException
operationException = exceptionBase MeowTypeMismatch . Text.append "Invalid operands for operation: "

divisionByZeroException :: (MonadIO m) => [MeowAtom] -> m CatException
divisionByZeroException = exceptionBase MeowDivByZero "Invalid operation: Cannot divide by zero!"

unboundException :: (MonadIO m) => Text.Text -> [MeowAtom] -> m CatException
unboundException = exceptionBase MeowUnboundKey . Text.append "Unbound key: "

notABoxException :: (MonadIO m) => [MeowAtom] -> m CatException
notABoxException = exceptionBase MeowNotBox 
    "Invalid operation: Attempted to perform lookup on a value that isn't a box."

notAPropertyException :: (MonadIO m) => Text.Text -> [MeowAtom] -> m CatException
notAPropertyException = exceptionBase MeowNotProperty . Text.append
    "Key provided is not an existent property in box: "

notAnIdentifier :: (MonadIO m) => [MeowAtom] -> m CatException
notAnIdentifier = exceptionBase MeowNotIdentifier "Value providied is not a valid identifier!"
