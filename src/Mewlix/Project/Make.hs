{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Project.Make
( ProjectMaker(..)
, make
-- Re-exports:
, liftIO
, throwError
, catchError
) where

import Control.Monad.Except (ExceptT, MonadError(throwError, catchError), runExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))

newtype ProjectMaker a = ProjectMaker { runProjectMaker :: ExceptT String IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError String
             )

make :: ProjectMaker a -> IO (Either String a)
make = runExceptT . runProjectMaker
