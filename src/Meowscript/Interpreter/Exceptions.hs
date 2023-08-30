{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Interpreter.Exceptions
( exceptionBase
, operationException
, divisionByZeroException
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
