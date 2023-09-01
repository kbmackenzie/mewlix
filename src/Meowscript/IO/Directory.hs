{-# LANGUAGE LambdaCase #-}

module Meowscript.IO.Directory
( localPath
, safeMoveDirectory
, safeRemoveDirectory
, safeRenameDirectory
, safeCurrentDirectory
, safeMakeDirectory
, safeDirectoryExists
, safeInspectDirectory
) where

import qualified Data.Text as Text
import Control.Exception (try, IOException)
import System.Directory (getCurrentDirectory
    , listDirectory
    , doesDirectoryExist
    , createDirectory
    , removeDirectory
    , renameDirectory)
import System.FilePath (dropFileName, (</>))


{- Directory Handling -}
----------------------------------------------------------
localPath :: FilePath -> FilePath -> FilePath
{-# INLINABLE localPath #-}
localPath path = (dropFileName path </>)

safeDirectoryFunc :: (FilePath -> IO ()) -> FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeDirectoryFunc #-}
safeDirectoryFunc f path = (try (f path) :: IO (Either IOException ())) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right _) -> (return . Right) ()

safeMoveDirectory :: FilePath -> FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeMoveDirectory #-}
safeMoveDirectory = safeDirectoryFunc . renameDirectory

safeRenameDirectory :: FilePath -> String -> IO (Either Text.Text ())
{-# INLINABLE safeRenameDirectory #-}
safeRenameDirectory path name = do
    let newPath = localPath path name
    safeDirectoryFunc (renameDirectory path) newPath

safeRemoveDirectory :: FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeRemoveDirectory #-}
safeRemoveDirectory = safeDirectoryFunc removeDirectory

safeCurrentDirectory :: IO (Either Text.Text Text.Text)
{-# INLINABLE safeCurrentDirectory #-}
safeCurrentDirectory = (try getCurrentDirectory :: IO (Either IOException FilePath)) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right directory) -> (return . Right . Text.pack) directory

safeMakeDirectory :: FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeMakeDirectory #-}
safeMakeDirectory path = (try (createDirectory path) :: IO (Either IOException ())) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right _) -> (return . Right) ()

safeDirectoryExists :: FilePath -> IO (Either Text.Text Bool)
{-# INLINABLE safeDirectoryExists #-}
safeDirectoryExists path = (try (doesDirectoryExist path) :: IO (Either IOException Bool)) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right exists) -> (return . Right) exists

safeInspectDirectory :: FilePath -> IO (Either Text.Text [FilePath])
{-# INLINABLE safeInspectDirectory #-}
safeInspectDirectory path = (try (listDirectory path) :: IO (Either IOException [FilePath])) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right contents) -> (return . Right) contents
