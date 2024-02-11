module Mewlix.Utils.IO
( readFile
, writeFile
, readDataFile
, writeFileS
) where

-- This module should always be imported qualified!

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as ByteUTF8
import qualified Data.Text.Encoding as ByteEncoding
import Data.Text.Encoding.Error (UnicodeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad ((>=>))
import Paths_Mewlix (getDataFileName)

readFile :: (MonadIO m) => FilePath -> m (Either UnicodeException Text)
readFile = liftIO . fmap ByteEncoding.decodeUtf8' . ByteString.readFile

writeFile :: (MonadIO m) => FilePath -> Text -> m ()
writeFile path = liftIO . ByteString.writeFile path . ByteEncoding.encodeUtf8

readDataFile :: (MonadIO m) => FilePath -> m (Either UnicodeException Text)
readDataFile = liftIO . getDataFileName >=> readFile

writeFileS :: (MonadIO m) => FilePath -> String -> m ()
writeFileS path = liftIO . ByteString.writeFile path . ByteUTF8.fromString
