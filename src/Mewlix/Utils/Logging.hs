module Mewlix.Utils.Logging
( hPutUtil
, writeStdout
, writeStderr
) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import System.IO (Handle, stdout, stderr)
import Control.Monad.IO.Class (MonadIO, liftIO)

hPutUtil :: (MonadIO m) => Handle -> Text -> m ()
hPutUtil handle = liftIO . TextIO.hPutStr handle

writeStdout :: (MonadIO m) => Text -> m ()
writeStdout = hPutUtil stdout

writeStderr :: (MonadIO m) => Text -> m ()
writeStderr = hPutUtil stderr
