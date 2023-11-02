{-# LANGUAGE OverloadedStrings #-}

module Mewlix.IO.Print
( printText
, printTextLn
, printError
, printErrorLn
, colorPrint
, printException
) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO (hFlush, stdout, stderr)
import System.Console.ANSI.Types
import qualified System.Console.ANSI as Console

{- IO Utils -}
----------------------------------------------------------
printText :: Text.Text -> IO ()
{-# INLINABLE printText #-}
printText line = TextIO.putStr line >> hFlush stdout

printTextLn :: Text.Text -> IO ()
{-# INLINABLE printTextLn #-}
printTextLn line = TextIO.putStrLn line >> hFlush stdout

printError :: Text.Text -> IO ()
{-# INLINABLE printError #-}
printError line = TextIO.hPutStr stderr line >> hFlush stderr

printErrorLn :: Text.Text -> IO ()
{-# INLINABLE printErrorLn #-}
printErrorLn line = TextIO.hPutStrLn stderr line >> hFlush stderr


{- Console Colors -}
----------------------------------------------------------
colorPrint :: Color -> Text.Text -> IO ()
{-# INLINABLE colorPrint #-}
colorPrint color text = do
    Console.setSGR [SetColor Foreground Vivid color, SetConsoleIntensity BoldIntensity]
    printText text
    Console.setSGR [Reset]
    return ()


{- Mewlix Exceptions -}
----------------------------------------------------------
printException :: Text.Text -> IO ()
printException txt
    | "[" `Text.isPrefixOf` txt = do
        let (exception, message) = splitAtExc ']' txt 
        colorPrint Magenta exception
        printTextLn message
    | otherwise = printErrorLn txt

splitAtExc :: Char -> Text.Text -> (Text.Text, Text.Text)
splitAtExc char str = let (a, b) = Text.span (/= char) str in if Text.null b
     then (a, b)
     else (a `Text.snoc` ']', Text.tail b)
