{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Compiler.Context
(
) where

import Mewlix.Abstract.AST
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Control.Monad.Reader (ReaderT, MonadReader, ask, asks, local)
import Control.Monad.Except (Except, MonadError, throwError, catchError)

data MewlixContext = MewlixContext -- todo
data MewlixError   = MewlixError   -- todo

newtype Transpiler a = Transpiler { runTranspiler :: ReaderT MewlixContext (Except MewlixError) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader MewlixContext
             , MonadError  MewlixError )
