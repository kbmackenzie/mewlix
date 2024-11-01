module Mewlix.Utils.Json
( parseJson
, serializeJson
) where

import Data.Aeson (eitherDecodeStrict, encode, FromJSON, ToJSON)
import Data.ByteString (ByteString, toStrict)

parseJson :: (FromJSON a) => ByteString -> Either String a
parseJson = eitherDecodeStrict

serializeJson :: (ToJSON a) => a -> ByteString
serializeJson = toStrict . encode
