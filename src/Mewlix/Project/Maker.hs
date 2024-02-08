{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Project.Maker
( ProjectMaker(..)
, makeProject
-- Re-exports:
, liftIO
, throwError
, catchError
) where

import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))

newtype ProjectMaker a = ProjectMaker { runProjectMaker :: ExceptT String IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError String
             )

makeProject :: ProjectMaker a -> IO (Either String a)
makeProject = runExceptT . runProjectMaker
