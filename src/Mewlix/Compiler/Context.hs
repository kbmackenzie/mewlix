{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Compiler.Context
( MewlixContext(..)
, MewlixExcept(..)
, Transpiler(..)
, CatException(..)
-- Lenses:
, contextBindingsL
, contextModulesL
, exceptionTypeL
, exceptionMessageL
-- Re-exports:
, ask
, asks
, local
, throwError
, catchError
) where

import Mewlix.Abstract.AST (Module)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Mewlix.Utils.Types (FilePathT)
import Lens.Micro.Platform (makeLensesFor)
import Control.Monad.Reader (ReaderT, MonadReader, ask, asks, local)
import Control.Monad.Except (Except, MonadError, throwError, catchError)

data MewlixContext = MewlixContext
    { contextBindings :: HashMap Text Text
    , contextModules  :: HashMap FilePathT Module }
    deriving (Show)

data MewlixExcept = MewlixExcept
    { exceptionType    :: CatException
    , exceptionMessage :: Text         }
    deriving (Show)

newtype Transpiler a = Transpiler { runTranspiler :: ReaderT MewlixContext (Except MewlixExcept) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader MewlixContext
             , MonadError  MewlixExcept )

data CatException =
      UnboundKey
    | TypeMismatch
    deriving (Show)

-- Lenses:
$(makeLensesFor
    [ ("contextBindings" , "contextBindingsL")
    , ("contextModules"  , "contextModulesL" ) ] ''MewlixContext)

$(makeLensesFor
    [ ("exceptionType"    , "exceptionTypeL"   )
    , ("exceptionMessage" , "exceptionMessageL") ] ''MewlixExcept)
