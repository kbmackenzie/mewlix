{-# LANGUAGE LambdaCase #-}

module Meowscript.Utils.IO
( printStr
, printStrLn
, colorPrint
, safeReadFile
, safeWriteFile
, safeAppendFile
, printError
) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO (hFlush, stdout)
import Control.Exception (try, IOException)
import System.Console.ANSI.Types
import qualified System.Console.ANSI as Console

printStr :: Text.Text -> IO ()
{-# INLINABLE printStr #-}
printStr line = TextIO.putStr line >> hFlush stdout

printStrLn :: Text.Text -> IO ()
{-# INLINABLE printStrLn #-}
printStrLn line = TextIO.putStrLn line >> hFlush stdout

----------------------------------------------------------

-- Console Colors --

colorPrint :: Color -> Text.Text -> IO ()
colorPrint color txt = do
    Console.setSGR [SetColor Foreground Dull color]
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

printError :: Text.Text -> IO ()
printError txt = do
    let (exception, message) = textSplit ']' txt 
    colorPrint errorColor exception
    printStrLn message

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
