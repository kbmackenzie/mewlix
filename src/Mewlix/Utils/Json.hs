module Mewlix.Utils.Json
( fromJson
) where

import Data.Aeson (eitherDecodeStrict, FromJSON)
import qualified Data.ByteString as ByteString
import Control.Monad.IO.Class (MonadIO, liftIO)

fromJson :: (MonadIO m, FromJSON a) => FilePath -> m (Either String a)
fromJson = fmap eitherDecodeStrict . liftIO . ByteString.readFile
