module Mewlix.Test.Utils
( readText
) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import Data.Text.Encoding.Error (UnicodeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)

readText :: (MonadIO m) => FilePath -> m (Either String Text)
readText = do
    let read_ :: (MonadIO m) => FilePath -> m (Either UnicodeException Text)
        read_ = liftIO . fmap Encoding.decodeUtf8' . ByteString.readFile

    let prettify :: UnicodeException -> String
        prettify e = "Couldn't decode string as UTF-8: " ++ show e

    fmap (first prettify) . read_


