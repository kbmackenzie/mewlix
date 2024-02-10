module Mewlix.Utils.IO
( readFile
, writeFile
) where

-- This module should always be imported qualified!

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import Data.Text.Encoding.Error (UnicodeException)
import Control.Monad.IO.Class (MonadIO, liftIO)

readFile :: (MonadIO m) => FilePath -> m (Either UnicodeException Text)
readFile = liftIO . fmap Encoding.decodeUtf8' . ByteString.readFile

writeFile :: (MonadIO m) => FilePath -> Text -> m ()
writeFile path = liftIO . ByteString.writeFile path . Encoding.encodeUtf8
