{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Logger
( LogLevel(..)
, logger
, rainbow
, catFace
) where

import System.IO (Handle, stdout, stderr, hPutChar, hPutStr, hPutStrLn, hGetEncoding, utf8)
import System.IO.Utf8 (withTerminalHandle)
import System.Console.ANSI
    ( SGR(..)
    , Color(..)
    , ConsoleLayer(..)
    , ColorIntensity(..)
    , ConsoleIntensity(..)
    , hSetSGR
    , hSupportsANSIColor
    )
import Prelude hiding (log)
import Control.Monad (foldM_)

data LogLevel = LogInfo | LogWarn | LogError

getStyles :: LogLevel -> [SGR]
getStyles level = case level of
    LogInfo  -> []
    LogWarn  -> [SetColor Foreground Vivid Yellow]
    LogError -> [SetColor Foreground Vivid Red]

getHandle :: LogLevel -> Handle
getHandle level = case level of
    LogInfo  -> stdout
    LogWarn  -> stderr
    LogError -> stderr

cycleColor :: Color -> Color
cycleColor color = case color of
    Red     -> Yellow
    Yellow  -> Green
    Green   -> Cyan
    Cyan    -> Blue
    Blue    -> Magenta
    Magenta -> Red
    _       -> Red

-- Write rainbow text to a handle.
rainbow :: Handle -> String -> IO ()
rainbow handle str = do
    let writeChar :: Color -> Char -> IO Color
        writeChar color char = do
            hSetSGR handle [SetColor Foreground Vivid color, SetConsoleIntensity BoldIntensity]
            hPutChar handle char
            return (cycleColor color)

    foldM_ writeChar Red str
    hSetSGR handle [Reset]

-- Write a cat face to a handle. 🐱
-- If UTF-8 encoding is supported, write cat emoji. If not, write '=^.x.^='.
catFace :: Handle -> IO ()
catFace handle = do
    encoding <- hGetEncoding handle
    -- A little hack-ish. The Show typeclass implementation for TextEncoding just
    -- returns the name of the encoding, so this is why I'm doing this.
    if fmap show encoding == Just (show utf8)
        then hPutChar handle '🐱'
        else hPutStr handle "=^.x.^="

logger :: LogLevel -> String -> IO ()
logger level message = do
    let handle = getHandle level
    let styles = getStyles level
    let put    = hPutStrLn handle

    supported <- hSupportsANSIColor handle
    withTerminalHandle handle $ if supported
        then do
            hSetSGR handle styles
            put message
            hSetSGR handle [Reset]
        else put message
