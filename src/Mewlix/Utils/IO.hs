module Mewlix.Utils.IO
( readFile
, writeFile
, readDataFile
) where

-- This module should always be imported qualified!

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import Data.Text.Encoding.Error (UnicodeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad ((>=>))
import Paths_Mewlix (getDataFileName)

readFile :: (MonadIO m) => FilePath -> m (Either UnicodeException Text)
readFile = liftIO . fmap Encoding.decodeUtf8' . ByteString.readFile

writeFile :: (MonadIO m) => FilePath -> Text -> m ()
writeFile path = liftIO . ByteString.writeFile path . Encoding.encodeUtf8

readDataFile :: (MonadIO m) => FilePath -> m (Either UnicodeException Text)
readDataFile = liftIO . getDataFileName >=> readFile
