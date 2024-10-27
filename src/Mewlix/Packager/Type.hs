{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Packager.Type
( Packager(..)
, packager
-- Re-exports:
, liftIO
, liftEither
, throwError
, catchError
) where

import Control.Monad.Except (ExceptT, MonadError(throwError, catchError), runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO(liftIO))

newtype Packager a = Packager
    { runPackager :: ExceptT String IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError String
             )

packager :: Packager a -> IO (Either String a)
packager = runExceptT . runPackager
