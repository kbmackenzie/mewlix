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

readFile :: FilePath -> IO (Either UnicodeException Text)
readFile = fmap Encoding.decodeUtf8' . ByteString.readFile

writeFile :: FilePath -> Text -> IO ()
writeFile path = ByteString.writeFile path . Encoding.encodeUtf8
