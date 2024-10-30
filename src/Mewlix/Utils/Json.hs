module Mewlix.Utils.Json
( parseJson
) where

import Data.Aeson (eitherDecodeStrict, FromJSON)
import Data.ByteString (ByteString)

parseJson :: (FromJSON a) => ByteString -> Either String a
parseJson = eitherDecodeStrict
