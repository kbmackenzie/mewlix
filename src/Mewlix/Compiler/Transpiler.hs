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
    { library :: HashSet Key    -- Special libraries included.
    , pretty  :: Bool           -- Prettify compiler output.
    , release :: Bool           -- Enable 'release mode': do not compile assertions.
    , noStd   :: Bool       }   -- Should the "std" yarn ball be implicitly imported?
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
    , pretty  = False
    , release = False
    , noStd   = False }
