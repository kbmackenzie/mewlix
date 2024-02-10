module Mewlix.Utils.IO
( readFileUTF8
) where

import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import Data.Text.Encoding.Error (UnicodeException)

readFileUTF8 :: FilePath -> IO (Either UnicodeException Text)
readFileUTF8 = fmap Encoding.decodeUtf8' . ByteString.readFile
