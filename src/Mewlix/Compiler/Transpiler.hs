{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Compiler.Transpiler
( TranspilerContext(..)
, Transpiler(..)
, transpile
, emptyContext
-- Re-exports:
, ask
, asks
, local
) where

import Mewlix.Abstract.Key (Key)
import Data.HashSet (HashSet)
import Control.Monad.Reader (MonadReader, Reader, ask, asks, local, runReader)

data TranspilerContext = TranspilerContext
    { library :: HashSet Key
    , noStd   :: Bool            
    , pretty  :: Bool        }
    deriving (Show);

newtype Transpiler a = Transpiler 
    { runTranspiler :: Reader TranspilerContext a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader TranspilerContext
             )

transpile :: TranspilerContext -> Transpiler a -> a
transpile context = flip runReader context . runTranspiler

emptyContext :: TranspilerContext
emptyContext = TranspilerContext
    { library = mempty
    , noStd   = False
    , pretty  = False }
