{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Compiler.Transpiler
( Transpiler(..)
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

data TransContext = TransContext
    { ctxBindings  :: HashSet Text
    , ctxConstants :: HashMap Text Text }
    deriving (Show);

newtype TransError = TransError { getMessage :: Text }
    deriving (Show);

newtype Transpiler a = Transpiler 
    { runTranspiler :: ReaderT TransContext (Except TransError) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader TransContext
             , MonadError TransError
             )

transpile :: TransContext -> Transpiler a -> Either TransError a
transpile ctx = runExcept . flip runReaderT ctx . runTranspiler;
