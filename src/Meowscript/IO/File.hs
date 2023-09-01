{-# LANGUAGE LambdaCase #-}

module Meowscript.IO.File
( safeReadFile
, safeWriteFile
, safeAppendFile
, safeMoveFile
, safeRenameFile
, safeRemoveFile
, safeDoesFileExist
) where

import Meowscript.IO.Directory
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Control.Exception (try, IOException)
import System.Directory (doesFileExist, renameFile, removeFile)

{- File Handling -}
----------------------------------------------------------
safeFileAction :: (FilePath -> IO ()) -> FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeFileAction #-}
safeFileAction f path = (try (f path) :: IO (Either IOException ())) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right _) -> (return . Right) ()

safeReadFile :: FilePath -> IO (Either Text.Text Text.Text)
{-# INLINABLE safeReadFile #-}
safeReadFile x = (try $ TextIO.readFile x :: IO (Either IOException Text.Text)) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right contents) -> (return . Right) contents
    
safeWriteFile :: Text.Text -> FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeWriteFile #-}
safeWriteFile contents = safeFileAction (`TextIO.writeFile` contents)

safeAppendFile :: Text.Text -> FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeAppendFile #-}
safeAppendFile contents = safeFileAction (`TextIO.appendFile` contents)

safeMoveFile :: FilePath -> FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeMoveFile #-}
safeMoveFile = safeFileAction . renameFile

safeRenameFile :: FilePath -> String -> IO (Either Text.Text ())
{-# INLINABLE safeRenameFile #-}
safeRenameFile path name = do
    let newPath = localPath path name
    safeFileAction (renameFile path) newPath

safeRemoveFile :: FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeRemoveFile #-}
safeRemoveFile = safeFileAction removeFile

safeDoesFileExist :: FilePath -> IO (Either Text.Text Bool)
{-# INLINABLE safeDoesFileExist #-}
safeDoesFileExist path = (try (doesFileExist path) :: IO (Either IOException Bool)) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right exists) -> (return . Right) exists
