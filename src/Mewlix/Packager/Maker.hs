{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Packager.Maker
( PackageMaker(..)
, packageMake
-- Re-exports:
, liftIO
, throwError
, catchError
) where

-- todo: rename PackageMaker to Packager. rename this moduule to just 'Type'.

import Control.Monad.Except (ExceptT, MonadError(throwError, catchError), runExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))

newtype PackageMaker a = PackageMaker
    { runPackageMaker :: ExceptT String IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError String
             )

packageMake :: PackageMaker a -> IO (Either String a)
packageMake = runExceptT . runPackageMaker
