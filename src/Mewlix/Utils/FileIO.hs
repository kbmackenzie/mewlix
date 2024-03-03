module Mewlix.Utils.FileIO
( readFileT
, readFileB
, writeFileT
, writeFileB
, writeFileS
, writeFileBL
, readDataFile
, copyFile
, copyDataFile
, appendFileT
, extractZip
, extractDataFile
) where

-- This module should always be imported qualified!

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.ByteString.UTF8 as ByteUTF8
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

readFileT :: (MonadIO m) => FilePath -> m (Either UnicodeException Text)
readFileT = liftIO . fmap ByteEncoding.decodeUtf8' . ByteString.readFile

readFileB :: (MonadIO m) => FilePath -> m ByteString
readFileB = liftIO . ByteString.readFile

writeFileT :: (MonadIO m) => FilePath -> Text -> m ()
writeFileT path = liftIO . ByteString.writeFile path . ByteEncoding.encodeUtf8

writeFileB :: (MonadIO m) => FilePath -> ByteString -> m ()
writeFileB = (liftIO .) . ByteString.writeFile

writeFileS :: (MonadIO m) => FilePath -> String -> m ()
writeFileS path = liftIO . ByteString.writeFile path . ByteUTF8.fromString

writeFileBL :: (MonadIO m) => FilePath -> ByteStringL.ByteString -> m ()
writeFileBL = (liftIO .) . ByteStringL.writeFile

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

appendFileT :: (MonadIO m) => FilePath -> Text -> m ()
appendFileT path = liftIO . ByteString.appendFile path . ByteEncoding.encodeUtf8

extractZip :: (MonadIO m) => FilePath -> FilePath -> m ()
extractZip zipPath = withArchive zipPath . unpackInto

extractDataFile :: (MonadIO m) => FilePath -> FilePath -> m ()
extractDataFile dataFile targetPath = do
    zipPath <- liftIO (getDataFileName dataFile)
    extractZip zipPath targetPath
