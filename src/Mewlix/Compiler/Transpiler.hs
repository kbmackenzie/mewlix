{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Control.Monad.Reader (MonadReader, Reader, ask, asks, local, runReader)

data TranspilerContext = TranspilerContext
    { imports :: HashMap Key Text
    , noStd   :: Bool            
    , pretty  :: Bool             }
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

emptyContext :: TranspilerContext
emptyContext = TranspilerContext
    { imports = mempty
    , noStd   = False
    , pretty  = False }
