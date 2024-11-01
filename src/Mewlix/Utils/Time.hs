module Mewlix.Utils.Time
( getPosixTimeInSeconds
) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Time.Clock.POSIX (getPOSIXTime)

getPosixTimeInSeconds :: (MonadIO m) => m Integer
getPosixTimeInSeconds = fmap round (liftIO getPOSIXTime)
