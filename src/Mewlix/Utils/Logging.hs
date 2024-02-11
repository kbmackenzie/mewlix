module Mewlix.Utils.Logging
( writeStdout
, writeStderr
) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO (Handle, stdout, stderr)
import Control.Monad.IO.Class (MonadIO, liftIO)

hPutUtil :: (MonadIO m) => Handle -> String -> m ()
hPutUtil handle = liftIO . TextIO.hPutStr handle . Text.pack

writeStdout :: (MonadIO m) => String -> m ()
writeStdout = hPutUtil stdout

writeStderr :: (MonadIO m) => String -> m ()
writeStderr = hPutUtil stderr
