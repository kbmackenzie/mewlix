module Test.Utils
( readText
) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import Data.Text.Encoding.Error (UnicodeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import System.Directory (removeFile)
import Control.Exception (throwIO, catch)
import System.IO.Error (isDoesNotExistError)

readText :: (MonadIO m) => FilePath -> m (Either String Text)
readText = do
    let read_ :: (MonadIO m) => FilePath -> m (Either UnicodeException Text)
        read_ = liftIO . fmap Encoding.decodeUtf8' . ByteString.readFile

    let prettify :: UnicodeException -> String
        prettify e = "Couldn't decode string as UTF-8: " ++ show e

    fmap (first prettify) . read_


