{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Utils.IO
( printStr
, printStrLn
, colorPrint
, safeReadFile
, safeWriteFile
, safeAppendFile
, safeCurrentDir
, printErr
, printErrLn
, printExc
) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO (hFlush, stdout, stderr)
import Control.Exception (try, IOException)
import System.Console.ANSI.Types
import qualified System.Console.ANSI as Console
import System.Directory (getCurrentDirectory)

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

safeWriteFile :: FilePath -> Text.Text -> IO (Either Text.Text ())
{-# INLINABLE safeWriteFile #-}
safeWriteFile path contents = (try $ TextIO.writeFile path contents :: IO (Either IOException ())) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right _) -> (return . Right) ()

safeAppendFile :: FilePath -> Text.Text -> IO (Either Text.Text ())
{-# INLINABLE safeAppendFile #-}
safeAppendFile path contents = (try $ TextIO.appendFile path contents :: IO (Either IOException ())) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right _) -> (return . Right) ()


----------------------------------------------------------

-- Directory Handling --

safeCurrentDir :: IO (Either Text.Text Text.Text)
{-# INLINABLE safeCurrentDir #-}
safeCurrentDir = (try getCurrentDirectory :: IO (Either IOException String)) >>= \case
    (Left exception) -> (return . Left . Text.pack . show) exception
    (Right directory) -> (return . Right . Text.pack) directory
