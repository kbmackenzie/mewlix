{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Utils.IO
( safelyRun
, readFileBytes
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
, getFileModTime
, compareFilesModTime
, EntryTag(..)
, exists
) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Encoding as TextEncoding
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text.Encoding.Error (UnicodeException)
import Control.Monad.Except (MonadError, liftEither, catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Paths_mewlix (getDataFileName)
import Codec.Archive.Zip (withArchive, unpackInto)
import System.Directory
    ( createDirectoryIfMissing
    , removeFile
    , copyFile
    , getModificationTime
    , doesFileExist
    , doesDirectoryExist
    , doesPathExist
    )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Exception (IOException, catch)
import System.IO.Error (isDoesNotExistError)

safelyRun :: (MonadIO m, MonadError String m) => IO a -> String -> m a
safelyRun action context = (liftIO >=> liftEither) $ do
    fmap Right action `catch` \err -> do
        let message = concat [context, " | error: ", show (err :: IOException)]
        return (Left message)

prettyError :: String -> IOException -> String
prettyError message err = concat [message, "| error: ", show err]

readFileBytes :: (MonadIO m, MonadError String m) => FilePath -> m ByteString
readFileBytes path = safelyRun (ByteString.readFile path) context
    where context = "couldn't read file " ++ show path

readFileText :: (MonadIO m, MonadError String m) => FilePath -> m Text
readFileText path = do
    let failContext :: UnicodeException -> String
        failContext = ("couldn't decode string as UTF-8: " ++) . show

    contents <- readFileBytes path
    liftEither . first failContext $ TextEncoding.decodeUtf8' contents

getDataFile :: (MonadIO m, MonadError String m) => FilePath -> m FilePath
getDataFile path = safelyRun (getDataFileName path) context
    where context = "couldn't get data file " ++ show path

readDataFile :: (MonadIO m, MonadError String m) => FilePath -> m ByteString
readDataFile = getDataFile >=> readFileBytes

writeFileBytes :: (MonadIO m, MonadError String m) => FilePath -> ByteString -> m ()
writeFileBytes path contents = safelyRun (ByteString.writeFile path contents) context
    where context = "couldn't write file " ++ show path

writeFileText :: (MonadIO m, MonadError String m) => FilePath -> Text -> m ()
writeFileText path contents = safelyRun (TextIO.writeFile path contents) context
    where context = "couldn't write file " ++ show path

copyFileSafe :: (MonadIO m, MonadError String m) => FilePath -> FilePath -> m ()
copyFileSafe target destination = safelyRun (copyFile target destination) context
    where context = concat ["couldn't copy file ", show target, " to destination ", show destination]

copyDataFile :: (MonadIO m, MonadError String m) => FilePath -> FilePath -> m ()
copyDataFile target destination = getDataFile target >>= flip copyDataFile destination

extractZip :: (MonadIO m, MonadError String m) => FilePath -> FilePath -> m ()
extractZip archive destination = safelyRun extract context
    where extract :: IO ()
          extract = withArchive archive (unpackInto destination)

          context :: String
          context = concat ["couldn't extract archive ", show archive, " to ", show destination]

extractZipDataFile :: (MonadIO m, MonadError String m) => FilePath -> FilePath -> m ()
extractZipDataFile archive destination = getDataFile archive >>= flip extractZip destination

removeIfExists :: (MonadIO m, MonadError String m) => FilePath -> m ()
removeIfExists path = (liftIO >=> liftEither) $ do
    fmap Right (removeFile path) `catch` \err -> do
        if isDoesNotExistError err
            then (return . Right) ()
            else do
                let message = "couldn't remove file " ++ show path
                return . Left $ prettyError message err

createDirectory :: (MonadIO m, MonadError String m) => Bool -> FilePath -> m ()
createDirectory recursive path = safelyRun (createDirectoryIfMissing recursive path) context
    where detail  = if recursive then " recursively" else ""
          context = concat ["couldn't create directory", detail, ": ", show path]

getFileModTime :: (MonadIO m, MonadError String m) => FilePath -> m Integer
getFileModTime path = numberfy <$> safelyRun (getModificationTime path) context
    where numberfy = round . utcTimeToPOSIXSeconds
          context  = "couldn't get modification time for file " ++ show path

compareFilesModTime :: (MonadIO m, MonadError String m) => FilePath -> FilePath -> m Ordering
compareFilesModTime a b = do
    timeA <- getFileModTime a `catchError` const (return 0)
    timeB <- getFileModTime b `catchError` const (return 0)
    return $ timeA `compare` timeB

data EntryTag = Any | File | Directory deriving (Show)

exists :: (MonadIO m, MonadError String m) => EntryTag -> FilePath -> m Bool
exists tag path = safelyRun (predicate path) context
    where context = "couldn't check existence of path " ++ show path
          predicate = case tag of
                File      -> doesFileExist
                Directory -> doesDirectoryExist
                Any       -> doesPathExist
