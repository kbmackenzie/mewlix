{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Meowscript.Evaluate.Exception
( CatException(..)
, MeowException(..)
, MeowableException(..)
, showException
) where

import qualified Data.Text as Text
import Meowscript.Utils.Show (showT)

data CatException = CatException
    { exceptionType     :: MeowException
    , exceptionMessage  :: Text.Text     }

data MeowException =
      MeowUnexpected
    | MeowArity
    | MeowTypeMismatch
    | MeowUnboundKey
    | MeowDivByZero
    | MeowNotBox
    | MeowNotProperty
    | MeowNotIdentifier
    | MeowBadIO
    -- Uncatchable:
    | MeowBadImport
    deriving (Eq, Ord, Enum, Bounded)

instance Show MeowException where
    show x = (++ "Exception") $ case x of
        MeowUnexpected      -> "Unexpected"
        MeowArity           -> "Arity"
        MeowTypeMismatch    -> "Type"
        MeowUnboundKey      -> "Unbound"
        MeowDivByZero       -> "DivisionByZero"
        MeowNotBox          -> "InvalidBox"
        MeowNotProperty     -> "UnboundProperty"
        MeowNotIdentifier   -> "InvalidIdentifier"
        MeowBadIO           -> "IO"
        MeowBadImport       -> "Import"

-----------------------------------------------------------------------------------

class MeowableException a where
    toCatException :: a -> CatException
    fromCatException :: CatException -> a

instance MeowableException Text.Text where
    toCatException = CatException MeowUnexpected
    fromCatException = exceptionMessage

instance MeowableException CatException where
    toCatException = id
    fromCatException = id

instance MeowableException MeowException where
    toCatException = flip CatException Text.empty
    fromCatException = exceptionType

------------------------------------------------------------------------------------

showException :: CatException -> Text.Text
showException e = Text.concat [ "[", (showT . exceptionType) e , "] ", exceptionMessage e ]
