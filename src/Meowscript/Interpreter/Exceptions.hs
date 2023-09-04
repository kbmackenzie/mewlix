{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Interpreter.Exceptions
( exceptionBase
, operationException
, divisionByZeroException
, unboundException
, notABoxException
, notAPropertyException
, notAnIdentifier
, notAFunctionName
, arityException
, notAFuncionException
, importException
, importSyntaxException
, unexpectedException
) where

import Meowscript.Abstract.Atom
import Meowscript.Abstract.Prettify
import Meowscript.Evaluate.Exception
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text as Text

exceptionBase :: (MonadIO m) => MeowException -> Text.Text -> [MeowAtom] -> m CatException
exceptionBase meow text xs = do
    items <- mapM prettyMeow xs
    let message = Text.concat [ text, " | Terms: ", Text.intercalate ", " items ]
    return CatException { exceptionType = meow, exceptionMessage = message }

operationException :: (MonadIO m) => Text.Text -> [MeowAtom] -> m CatException
operationException = exceptionBase MeowTypeMismatch . Text.append "Invalid operands for operation: "

divisionByZeroException :: (MonadIO m) => [MeowAtom] -> m CatException
divisionByZeroException = exceptionBase MeowDivByZero "Invalid operation: Cannot divide by zero!"

unboundException :: Text.Text -> CatException
unboundException key = CatException MeowUnboundKey $ Text.concat
    [ "Unbound key: \"", key, "\"!" ]

notABoxException :: (MonadIO m) => [MeowAtom] -> m CatException
notABoxException = exceptionBase MeowNotBox 
    "Invalid operation: Attempted to perform lookup on a value that isn't a box."

notAPropertyException :: (MonadIO m) => Text.Text -> [MeowAtom] -> m CatException
notAPropertyException = exceptionBase MeowNotProperty . Text.append
    "Key provided is not an existent property in box: "

notAnIdentifier :: (MonadIO m) => [MeowAtom] -> m CatException
notAnIdentifier = exceptionBase MeowNotIdentifier "Value providied is not a valid identifier!"

notAFunctionName :: (MonadIO m) => [MeowAtom] -> m CatException
notAFunctionName = exceptionBase MeowNotIdentifier "Value provided is not a valid function identifier!"

arityException :: Text.Text -> Text.Text -> CatException
arityException message key = CatException MeowArity $ Text.concat
    [ message, " passed to function \"", key, "\"!" ]

notAFuncionException :: (MonadIO m) => [MeowAtom] -> m CatException
notAFuncionException = exceptionBase MeowTypeMismatch "Cannot invoke a non-callable value as a function!"

importException :: FilePath -> CatException
importException path = CatException MeowBadImport $ Text.concat
    [ "Could not import file \"", Text.pack path, "\": File not found!" ]

importSyntaxException :: FilePath -> Text.Text -> CatException
importSyntaxException path message = CatException MeowBadImport $ Text.concat
    [ "Could not import file \"", Text.pack path, "\" due to a syntax error:\n", message ]

unexpectedException :: Text.Text -> CatException
unexpectedException = CatException MeowUnexpected . flip Text.append pleaseContactTheDev

pleaseContactTheDev :: Text.Text
pleaseContactTheDev = " | Please contact the dev at @KBMackenzie on Github to report if you can!"
