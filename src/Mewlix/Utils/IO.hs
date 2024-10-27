{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Utils.IO
( readFileBytes
, readFileText
, readDataFile
, writeFileBytes
, writeFileText
, copyFileSafe
, copyDataFile
, extractZip
, extractZipDataFile
, removeIfExists
, createDirectory
) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Encoding as TextEncoding
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text.Encoding.Error (UnicodeException)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Paths_mewlix (getDataFileName)
import Codec.Archive.Zip (withArchive, unpackInto)
import System.Directory
    ( createDirectoryIfMissing
    , removeFile
    , copyFile
    )
import Control.Exception (IOException, catch)
import System.IO.Error (isDoesNotExistError)

fileError :: String -> IOException -> String
fileError message err = concat [message, "| error: ", show err]

readFileBytes :: (MonadIO m, MonadError String m) => FilePath -> m ByteString
readFileBytes path = (liftIO >=> liftEither) $ do
    fmap Right (ByteString.readFile path) `catch` \err -> do
        let message = "couldn't read file " ++ show path
        return . Left $ fileError message err

readFileText :: (MonadIO m, MonadError String m) => FilePath -> m Text
readFileText path = do
    let failContext :: UnicodeException -> String
        failContext = ("couldn't decode string as UTF-8: " ++) . show

    contents <- readFileBytes path
    liftEither . first failContext $ TextEncoding.decodeUtf8' contents

getDataFile :: (MonadIO m, MonadError String m) => FilePath -> m FilePath
getDataFile path = (liftIO >=> liftEither) $ do
    fmap Right (getDataFileName path) `catch` \err -> do
        let message = "couldn't get data file " ++ show path
        return . Left $ fileError message err

readDataFile :: (MonadIO m, MonadError String m) => FilePath -> m ByteString
readDataFile = getDataFile >=> readFileBytes

writeFileBytes :: (MonadIO m, MonadError String m) => FilePath -> ByteString -> m ()
writeFileBytes path contents = (liftIO >=> liftEither) $ do
    fmap Right (ByteString.writeFile path contents) `catch` \err -> do
        let message = "couldn't write file " ++ show path
        return . Left $ fileError message err

writeFileText :: (MonadIO m, MonadError String m) => FilePath -> Text -> m ()
writeFileText path contents = (liftIO >=> liftEither) $ do
    fmap Right (TextIO.writeFile path contents) `catch` \err -> do
        let message = "couldn't write file " ++ show path
        return . Left $ fileError message err

copyFileSafe :: (MonadIO m, MonadError String m) => FilePath -> FilePath -> m ()
copyFileSafe target destination = (liftIO >=> liftEither) $ do
    fmap Right (copyFile target destination) `catch` \err -> do
        let message = concat ["couldn't copy file ", show target, " to destination ", show destination]
        return . Left $ fileError message err

copyDataFile :: (MonadIO m, MonadError String m) => FilePath -> FilePath -> m ()
copyDataFile target destination = getDataFile target >>= flip copyDataFile destination

extractZip :: (MonadIO m, MonadError String m) => FilePath -> FilePath -> m ()
extractZip archive destination = (liftIO >=> liftEither) $ do
    let extract :: IO ()
        extract = withArchive archive (unpackInto destination)
    fmap Right extract `catch` \err -> do
        let message = concat ["couldn't extract archive ", show archive, " to ", show destination]
        return . Left $ fileError message err

extractZipDataFile :: (MonadIO m, MonadError String m) => FilePath -> FilePath -> m ()
extractZipDataFile archive destination = getDataFile archive >>= flip extractZip destination

removeIfExists :: (MonadIO m, MonadError String m) => FilePath -> m ()
removeIfExists path = (liftIO >=> liftEither) $ do
    fmap Right (removeFile path) `catch` \err -> do
        if isDoesNotExistError err
            then (return . Right) ()
            else do
                let message = "couldn't remove file " ++ show path
                return . Left $ fileError message err

createDirectory :: (MonadIO m, MonadError String m) => Bool -> FilePath -> m ()
createDirectory recursive path = (liftIO >=> liftEither) $ do
    fmap Right (createDirectoryIfMissing recursive path) `catch` \err -> do
        let detail  = if recursive then " recursively" else ""
        let message = concat ["couldn't create directory", detail, ": ", show path]
        return . Left $ fileError message err
