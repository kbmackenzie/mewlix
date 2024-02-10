module Mewlix.Utils.Json
( readJson
) where

import Data.Aeson (eitherDecodeStrict, FromJSON)
import qualified Data.ByteString as ByteString
import Control.Monad.IO.Class (MonadIO, liftIO)

readJson :: (MonadIO m, FromJSON a) => FilePath -> m (Either String a)
readJson = fmap eitherDecodeStrict . liftIO . ByteString.readFile
