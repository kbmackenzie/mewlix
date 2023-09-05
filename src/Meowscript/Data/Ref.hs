{-# LANGUAGE BangPatterns #-}

module Meowscript.Data.Ref
( Ref(..)
, newRef
, readRef
, writeRef
, modifyRef
, copyRef
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO(..))

newtype Ref a = Ref { getRef :: IORef a }

newRef :: (MonadIO m) => a -> m (Ref a)
{-# INLINE newRef #-}
newRef = fmap Ref . liftIO . newIORef

readRef :: (MonadIO m) => Ref a -> m a
{-# INLINE readRef #-}
readRef = liftIO . readIORef . getRef

writeRef :: (MonadIO m) => a -> Ref a -> m ()
{-# INLINE writeRef #-}
writeRef !a = liftIO . flip writeIORef a . getRef

-- A strict alternative to 'modifyIORef', as 'modifyIORef' is lazy and creates a lot of chunks.
-- A better monadic variation (a -> m a) might be necessary.
modifyRef :: (MonadIO m) => (a -> a) -> Ref a -> m ()
{-# INLINE modifyRef #-}
modifyRef !f !ref = do
    !a <- readRef ref
    let !a' = f a
    writeRef a' ref

copyRef :: (MonadIO m) => Ref a -> m (Ref a)
{-# INLINE copyRef #-}
copyRef = readRef >=> newRef
