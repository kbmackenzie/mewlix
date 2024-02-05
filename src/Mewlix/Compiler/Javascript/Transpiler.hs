{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Mewlix.Compiler.Javascript.Transpiler
( TranspilerContext(..)
, Transpiler(..)
, transpile
-- Re-exports:
, ask
, asks
, local
, throwError
, catchError
) where

import Mewlix.Abstract.Key (Key)
import Data.Text (Text)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.Except (MonadError, Except, throwError, catchError, runExcept)
import Lens.Micro.Platform (makeLensesFor)

data TranspilerContext = TranspilerContext
    { specialImports :: HashMap Key Text
    , placeholder    :: Key               }
    deriving (Show);

newtype Transpiler a = Transpiler 
    { runTranspiler :: ReaderT TranspilerContext (Except Text) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader TranspilerContext
             , MonadError Text
             )

transpile :: TranspilerContext -> Transpiler a -> Either Text a
transpile ctx = runExcept . flip runReaderT ctx . runTranspiler;
