{-# LANGUAGE LambdaCase #-}

module Mewlix.Utils.FileIO
( DecodeFunc
, decode
, readText
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
import qualified Data.Text.Encoding as Encoding
import Data.Text.Encoding.Error (UnicodeException, OnDecodeError, strictDecode)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad ((>=>), foldM)
import Paths_mewlix (getDataFileName)
import Conduit
    ( runConduitRes
    , sourceFile
    , sinkFile
    , (.|)
    )
import Codec.Archive.Zip (withArchive, unpackInto)
import System.Directory (removeFile)
import Control.Exception (throwIO, try, catch, evaluate)
import System.IO.Error (isDoesNotExistError)
import Mewlix.Utils.Maybe (hush)

type DecodeFunc = OnDecodeError -> ByteString -> Text

decoders :: [DecodeFunc]
decoders =
    [ Encoding.decodeUtf8With
    , Encoding.decodeUtf16LEWith
    , Encoding.decodeUtf16BEWith
    , Encoding.decodeUtf32LEWith
    , Encoding.decodeUtf32BEWith ]

decode :: (MonadIO m) => DecodeFunc -> ByteString -> m (Either UnicodeException Text)
decode dec = liftIO . try . evaluate . dec strictDecode

decode_ :: (MonadIO m) => DecodeFunc -> ByteString -> m (Maybe Text)
decode_ = (fmap hush .) . decode

tryDecode :: (MonadIO m) => ByteString -> m (Either String Text)
tryDecode str = do
    let run :: (MonadIO m) => Maybe Text -> DecodeFunc -> m (Maybe Text)
        run (Just x) _    = return (Just x)
        run _        func = decode_ func str

    foldM run mempty decoders >>= \case
        (Just x) -> return (Right x)
        Nothing  -> return . Left $ "Couldn't decode string!"

readText :: (MonadIO m) => FilePath -> m (Either String Text)
readText = liftIO . ByteString.readFile >=> tryDecode

writeText :: (MonadIO m) => FilePath -> Text -> m ()
writeText path = liftIO . ByteString.writeFile path . Encoding.encodeUtf8

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
