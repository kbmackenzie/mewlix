module Mewlix.Utils.Logging
( hPutUtil
, writeStdout
, writeStderr
) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import System.IO (Handle, stdout, stderr)
import Control.Monad.IO.Class (MonadIO, liftIO)

hPutUtil :: (MonadIO m) => Bool -> Handle -> Text -> m ()
hPutUtil newline handle = liftIO . put handle
    where put = if newline then TextIO.hPutStrLn else TextIO.hPutStr

writeStdout :: (MonadIO m) => Text -> m ()
writeStdout = hPutUtil True stdout

writeStderr :: (MonadIO m) => Text -> m ()
writeStderr = hPutUtil True stderr
