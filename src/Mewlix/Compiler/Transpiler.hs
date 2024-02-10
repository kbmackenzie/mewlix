{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Compiler.Transpiler
( TranspilerContext(..)
, Transpiler(..)
, transpile
-- Re-exports:
, ask
, asks
, local
) where

import Mewlix.Abstract.Key (Key)
import Data.Text (Text)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Mewlix.Abstract.Module (ModuleKey)
import Control.Monad.Reader (MonadReader, Reader, ask, asks, local, runReader)

data TranspilerContext = TranspilerContext
    { specialImports    :: HashMap Key Text
    , transpilerFlags   :: HashSet Text     }
    deriving (Show);

newtype Transpiler a = Transpiler 
    { runTranspiler :: Reader TranspilerContext a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader TranspilerContext
             )

transpile :: TranspilerContext -> Transpiler a -> a
transpile ctx = flip runReader ctx . runTranspiler;
