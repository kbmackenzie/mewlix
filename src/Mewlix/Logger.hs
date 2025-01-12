{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Logger
( LogLevel(..)
, logger
, rainbow
) where

import System.IO (Handle, stdout, stderr, hPutChar, hPutStrLn)
import Control.Monad.IO.Class (MonadIO, liftIO)
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

rainbow :: Handle -> String -> IO ()
rainbow handle str = liftIO $ do
    let writeChar :: Color -> Char -> IO Color
        writeChar color char = do
            hSetSGR handle [SetColor Foreground Vivid color, SetConsoleIntensity BoldIntensity]
            hPutChar handle char
            return (cycleColor color)

    foldM_ writeChar Red str
    hSetSGR handle [Reset]

logger :: (MonadIO m) => LogLevel -> String -> m ()
logger level message = liftIO $ do
    let handle = getHandle level
    let styles = getStyles level
    let put    = hPutStrLn handle

    supported <- hSupportsANSIColor handle
    if supported
        then do
            hSetSGR handle styles
            put message
            hSetSGR handle [Reset]
        else do
            put message
