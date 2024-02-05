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
) where

import Mewlix.Abstract.Key (Key)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Control.Monad.Reader (MonadReader, Reader, ask, asks, local, runReader)

data TranspilerContext = TranspilerContext
    { specialImports :: HashMap Key Text
    , placeholder    :: Key               }
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
