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
