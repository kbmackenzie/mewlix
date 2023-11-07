{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Interpreter.Exceptions
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
, notAClassException
, expectedBox
, expectedString
, argumentTypeException
, conversionException
, catOnComputer
, importException
, meowIOException
, importSyntaxException
, unexpectedException
) where

import Mewlix.Abstract.Meow
import Mewlix.Abstract.Prettify
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.IO.Class (MonadIO(..))

exceptionBase :: (MonadIO m) => MeowException -> Text -> [MeowPrim] -> m CatException
exceptionBase meow text xs = do
    items <- mapM prettyMeow xs
    let message = Text.concat [ text, " | Terms: ", Text.intercalate ", " items ]
    return CatException { exceptionType = meow, exceptionMessage = message }

operationException :: (MonadIO m) => Text -> [MeowPrim] -> m CatException
operationException = exceptionBase MeowTypeMismatch . Text.append "Invalid operands for operation: "

divisionByZeroException :: (MonadIO m) => [MeowPrim] -> m CatException
divisionByZeroException = exceptionBase MeowDivByZero "Invalid operation: Cannot divide by zero!"

unboundException :: Text -> CatException
unboundException key = CatException MeowUnboundKey $ Text.concat
    [ "Unbound key: \"", key, "\"!" ]

notABoxException :: (MonadIO m) => [MeowPrim] -> m CatException
notABoxException = exceptionBase MeowNotBox 
    "Invalid operation: Attempted to perform lookup on a value that isn't a box."

notAPropertyException :: (MonadIO m) => Text -> [MeowPrim] -> m CatException
notAPropertyException = exceptionBase MeowNotProperty . Text.append
    "Key provided is not an existent property in box: "

notAnIdentifier :: (MonadIO m) => [MeowPrim] -> m CatException
notAnIdentifier = exceptionBase MeowNotIdentifier "Value providied is not a valid identifier!"

notAFunctionName :: (MonadIO m) => [MeowPrim] -> m CatException
notAFunctionName = exceptionBase MeowNotIdentifier "Value provided is not a valid function identifier!"

arityException :: Text -> Text -> CatException
arityException message key = CatException MeowArity $ Text.concat
    [ message, " passed to function \"", key, "\"!" ]

notAFuncionException :: (MonadIO m) => [MeowPrim] -> m CatException
notAFuncionException = exceptionBase MeowTypeMismatch "Cannot invoke a non-callable value as a function!"

notAClassException :: (MonadIO m) => [MeowPrim] -> m CatException
notAClassException = exceptionBase MeowTypeMismatch "Value provided is not a class!"

expectedBox :: (MonadIO m) => [MeowPrim] -> m CatException
expectedBox = exceptionBase MeowTypeMismatch "Expected box; received a different value!"

expectedString :: (MonadIO m) => [MeowPrim] -> m CatException
expectedString = exceptionBase MeowTypeMismatch "Expected string; received a different value!"

argumentTypeException :: (MonadIO m) => Text -> [MeowPrim] -> m CatException
argumentTypeException key = exceptionBase MeowTypeMismatch $ Text.concat
    [ "Invalid arguments passed to function \"", key, "\"!" ]

conversionException :: Text -> Text -> CatException
conversionException target message = CatException MeowTypeMismatch $ Text.concat
    [ "Could not convert string to ", target,": ", message ]

catOnComputer :: Text -> CatException
catOnComputer = CatException MeowCatOnComputer

importException :: FilePath -> CatException
importException path = CatException MeowBadImport $ Text.concat
    [ "Could not import file \"", Text.pack path, "\": File not found!" ]

meowIOException :: Text -> Text -> CatException
meowIOException action message = CatException MeowBadIO $ Text.concat
    [ "Couldn't perform action \"", action, "\":", message ]

importSyntaxException :: FilePath -> Text -> CatException
importSyntaxException path message = CatException MeowBadImport $ Text.concat
    [ "Could not import file \"", Text.pack path, "\" due to a syntax error:\n", message ]

unexpectedException :: Text -> CatException
unexpectedException = CatException MeowUnexpected . flip Text.append pleaseContactTheDev

pleaseContactTheDev :: Text
pleaseContactTheDev = " | Please contact the dev at @KBMackenzie on Github to report this error if you can!"
