module Mewlix.Utils.FileIO
( readText
, writeText
, readBytes
, writeBytes
, readDataFile
, copyFile
, copyDataFile
, extractZip
, extractDataFile
, removeIfExists
) where

-- This module should always be imported qualified!

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as ByteEncoding
import Data.Text.Encoding.Error (UnicodeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad ((>=>))
import Paths_mewlix (getDataFileName)
import Conduit
    ( runConduitRes
    , sourceFile
    , sinkFile
    , (.|)
    )
import Codec.Archive.Zip (withArchive, unpackInto)
import System.Directory (removeFile)
import Control.Exception (throwIO, catch)
import System.IO.Error (isDoesNotExistError)

readText :: (MonadIO m) => FilePath -> m (Either UnicodeException Text)
readText = liftIO . fmap ByteEncoding.decodeUtf8' . ByteString.readFile

writeText :: (MonadIO m) => FilePath -> Text -> m ()
writeText path = liftIO . ByteString.writeFile path . ByteEncoding.encodeUtf8

readBytes :: (MonadIO m) => FilePath -> m ByteString
readBytes = liftIO . ByteString.readFile

writeBytes :: (MonadIO m) => FilePath -> ByteString -> m ()
writeBytes path = liftIO . ByteString.writeFile path

readDataFile :: (MonadIO m) => FilePath -> m ByteString
readDataFile = liftIO . getDataFileName >=> liftIO . ByteString.readFile

copyFile :: (MonadIO m) => FilePath -> FilePath -> m ()
copyFile file targetPath = liftIO . runConduitRes
     $ sourceFile file
    .| sinkFile targetPath

copyDataFile :: (MonadIO m) => FilePath -> FilePath -> m ()
copyDataFile dataFile targetPath = do
    dataPath <- liftIO (getDataFileName dataFile)
    copyFile dataPath targetPath

extractZip :: (MonadIO m) => FilePath -> FilePath -> m ()
extractZip zipPath = withArchive zipPath . unpackInto

extractDataFile :: (MonadIO m) => FilePath -> FilePath -> m ()
extractDataFile dataFile targetPath = do
    zipPath <- liftIO (getDataFileName dataFile)
    extractZip zipPath targetPath

removeIfExists :: (MonadIO m) => FilePath -> m ()
removeIfExists path = liftIO $
    removeFile path `catch` \e -> if isDoesNotExistError e
        then return ()
        else throwIO e
