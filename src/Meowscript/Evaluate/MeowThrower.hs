module Meowscript.Evaluate.MeowThrower
( MeowThrower(..)
, liftEither
) where

import Meowscript.Evaluate.Exception
import Control.Monad.Except (ExceptT, throwError, catchError)

class (Monad m) => MeowThrower m where
    throwException :: CatException -> m a
    catchException :: m a -> (CatException -> m a) -> m a

instance (Monad m, MeowableException e) => MeowThrower (ExceptT e m) where
    throwException = throwError . fromCatException
    catchException m f = catchError m (f . toCatException)

instance (MeowableException e) => MeowThrower (Either e) where
    throwException = Left . fromCatException
    catchException m f = catchError m (f . toCatException)

liftEither :: (MeowThrower m, MeowableException e) => Either e a -> m a
{-# INLINE liftEither #-}
liftEither (Left e) = (throwException . toCatException) e
liftEither (Right a) = return a
