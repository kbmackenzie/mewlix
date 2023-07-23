{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Utils.IO
( printStr
, printStrLn
, colorPrint
, printExc
, safeReadFile
, safeWriteFile
, safeAppendFile
, safeMoveFile
, safeRenameFile
, safeRemoveFile
, safeDoesFileExist
, safeDoesFileExist'
, safeCurrentDir
, safeMoveDir
, safeRenameDir
, safeRemoveDir
, safeMakeDirectory
, safeDirectoryExists
, safeInspectDirectory
, printErr
, printErrLn
, localPath
) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO (hFlush, stdout, stderr)
import Control.Exception (try, IOException)
import System.Console.ANSI.Types
import qualified System.Console.ANSI as Console
import System.Directory (getCurrentDirectory
    , doesFileExist
    , listDirectory
    , doesDirectoryExist
    , renameFile
    , removeFile
    , createDirectory
    , removeDirectory
    , renameDirectory)
import System.FilePath (dropFileName, (</>))
import Data.Either (fromRight)

printStr :: Text.Text -> IO ()
{-# INLINABLE printStr #-}
printStr line = TextIO.putStr line >> hFlush stdout

printStrLn :: Text.Text -> IO ()
{-# INLINABLE printStrLn #-}
printStrLn line = TextIO.putStrLn line >> hFlush stdout

printErr :: Text.Text -> IO ()
{-# INLINABLE printErr #-}
printErr line = TextIO.hPutStr stderr line >> hFlush stderr

printErrLn :: Text.Text -> IO ()
{-# INLINABLE printErrLn #-}
printErrLn line = TextIO.hPutStrLn stderr line >> hFlush stderr

----------------------------------------------------------

-- Console Colors --

colorPrint :: Color -> Text.Text -> IO ()
{-# INLINABLE colorPrint #-}
colorPrint color txt = do
    Console.setSGR [SetColor Foreground Vivid color, SetConsoleIntensity BoldIntensity]
    printStr txt
    Console.setSGR [Reset]
    return ()

----------------------------------------------------------

-- Console Errors --

errorColor :: Color
errorColor = Magenta

textSplit :: Char -> Text.Text -> (Text.Text, Text.Text)
textSplit char str = (a', b')
    where (a, b) = Text.span (/= char) str
          a' = if Text.null b then a else a `Text.snoc` Text.head b
          b' = if Text.null b then b else Text.tail b

printExc :: Text.Text -> IO ()
printExc txt
    | "[" `Text.isPrefixOf` txt = do
        let (exception, message) = textSplit ']' txt 
        colorPrint errorColor exception
        printStrLn message
    | otherwise = printErrLn txt

----------------------------------------------------------

-- File Handling --

safeReadFile :: FilePath -> IO (Either Text.Text Text.Text)
{-# INLINABLE safeReadFile #-}
safeReadFile x = (try $ TextIO.readFile x :: IO (Either IOException Text.Text)) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right contents) -> (return . Right) contents
    
safeFileAction :: (FilePath -> IO ()) -> FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeFileAction #-}
safeFileAction f path = (try (f path) :: IO (Either IOException ())) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right _) -> (return . Right) ()

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

----------------------------------------------------------

-- Directory Handling --

safeDirAction :: (FilePath -> IO ()) -> FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeDirAction #-}
safeDirAction = safeFileAction

safeMoveDir :: FilePath -> FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeMoveDir #-}
safeMoveDir = safeDirAction . renameDirectory

safeRenameDir :: FilePath -> String -> IO (Either Text.Text ())
{-# INLINABLE safeRenameDir #-}
safeRenameDir path name = do
    let newPath = localPath path name
    safeFileAction (renameDirectory path) newPath

safeRemoveDir :: FilePath -> IO (Either Text.Text ())
{-# INLINABLE safeRemoveDir #-}
safeRemoveDir = safeDirAction removeDirectory

safeCurrentDir :: IO (Either Text.Text Text.Text)
{-# INLINABLE safeCurrentDir #-}
safeCurrentDir = (try getCurrentDirectory :: IO (Either IOException FilePath)) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right directory) -> (return . Right . Text.pack) directory

safeDoesFileExist :: FilePath -> IO (Either Text.Text Bool)
{-# INLINABLE safeDoesFileExist #-}
safeDoesFileExist path = (try (doesFileExist path) :: IO (Either IOException Bool)) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right exists) -> (return . Right) exists

-- A variation that muffles exceptions, returning False in case they happen.
-- Not exactly great, but works for Meowr's purposes.
safeDoesFileExist' :: FilePath -> IO Bool
{-# INLINABLE safeDoesFileExist' #-}
safeDoesFileExist' = fmap (fromRight False) . safeDoesFileExist

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

localPath :: FilePath -> FilePath -> FilePath
{-# INLINABLE localPath #-}
localPath path = (dropFileName path </>)
