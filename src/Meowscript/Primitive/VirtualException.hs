{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Meowscript.Bytecode.VirtualException
( CatException(..)
, MeowException(..)
, showException
) where

import qualified Data.Text as Text
import Meowscript.Utils.Show (showT)

data CatException = CatException
    { exceptionType     :: MeowException
    , exceptionMessage  :: Text.Text     }

data MeowException =
      MeowVM
    | MeowArity
    | MeowTypeMismatch
    | MeowUnboundKey
    | MeowDivByZero
    | MeowBadIO
    | MeowBadImport
    deriving (Eq, Ord, Enum, Bounded)

instance Show MeowException where
    show x = (++ "Exception") $ case x of
        MeowVM              -> "VM"
        MeowArity           -> "Arity"
        MeowTypeMismatch    -> "Type"
        MeowUnboundKey      -> "Unbound"
        MeowDivByZero       -> "DivisionByZero"
        MeowBadIO           -> "IO"
        MeowBadImport       -> "Import"

showException :: CatException -> Text.Text
showException e = let excType = showT (exceptionType e) in Text.concat
    [ "[", excType, "] ", exceptionMessage e ]
