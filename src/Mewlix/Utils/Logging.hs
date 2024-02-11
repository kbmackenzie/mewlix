module Mewlix.Utils.Logging
( writeStdout
, writeStderr
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text as TextIO
import System.IO (Handle, stdout, stderr)
import Control.Monad.IO.Class (MonadIO, liftIO)

hPutUtil :: (MonadIO m) => Handle -> String -> m ()
hPutUtil handle = liftIO . ByteString.hPut handle . ByteUTF8.fromString

writeStdout :: (MonadIO m) => String -> m ()
writeStdout = hPutUtil stdout

writeStderr :: (MonadIO m) => String -> m ()
writeStderr = hPutUtil stderr
