{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Analysis.Analyzer
( Analyzer(..)
, AnalysisError(..)
, AnalysisErrorType(..)
, analyze
-- Re-exports:
, throwError
, catchError
) where

import Data.Text (Text)
import Control.Monad.Except
    ( MonadError
    , Except
    , runExcept
    , throwError
    , catchError
    )

data AnalysisErrorType =
      UnboundKey
    | TypeMismatch
    deriving (Eq, Ord, Show, Enum, Bounded)

data AnalysisError = AnalysisError
    { analysisErrorType     :: AnalysisErrorType
    , analysisErrorMessage  :: Text              }
    deriving (Show)

newtype Analyzer a = Analyzer { runAnalyzer :: Except AnalysisError a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError AnalysisError
             )

analyze :: Analyzer a -> Either AnalysisError a
analyze = runExcept . runAnalyzer
