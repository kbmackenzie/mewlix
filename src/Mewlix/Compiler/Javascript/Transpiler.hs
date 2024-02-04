{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Mewlix.Compiler.Javascript.Transpiler
( TransContext(..)
, Transpiler(..)
, transpile
-- Re-exports:
, ask
, asks
, local
, throwError
, catchError
) where

import Data.Text (Text)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.Except (MonadError, Except, throwError, catchError, runExcept)
import Lens.Micro.Platform (makeLensesFor)

data TransContext = TransContext
    deriving (Show);

newtype Transpiler a = Transpiler 
    { runTranspiler :: ReaderT TransContext (Except Text) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader TransContext
             , MonadError Text
             )

transpile :: TransContext -> Transpiler a -> Either Text a
transpile ctx = runExcept . flip runReaderT ctx . runTranspiler;
