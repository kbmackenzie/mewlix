{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Mewlix.Compiler.Transpiler
( TransContext(..)
, TransError(..)
, Transpiler(..)
, transpile
-- Lens:
, globalBindingsL
, globalConstantsL
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
    { globalBindings  :: HashSet Text
    , globalConstants :: HashMap Text Text }
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

{- Lens: -}
--------------------------------------------------------------------
$(makeLensesFor
    [ ("globalBindings"  , "globalBindingsL"  )
    , ("globalConstants" , "globalConstantsL" ) ] ''TransContext)
