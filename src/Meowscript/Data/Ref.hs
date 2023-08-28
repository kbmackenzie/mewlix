module Meowscript.Data.Ref
(
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Control.Monad.IO.Class (MonadIO(..))

newtype Ref a = Ref { getRef :: IORef a }

newRef :: (MonadIO m) => a -> m (Ref a)
newRef = fmap Ref . liftIO . newIORef

readRef :: (MonadIO m) => Ref a -> m a
readRef = liftIO . readIORef . getRef

writeRef :: (MonadIO m) => Ref a -> a -> m ()
writeRef = (liftIO .) . writeIORef . getRef
