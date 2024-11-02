{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Logger
( LogData(..)
, LogType(..)
, logger
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO (Handle, stdout, stderr)
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

data LogType = LogInfo | LogError

data LogData = LogData
    { logType    :: LogType
    , logPrefix  :: Maybe Text
    , logMessage :: Text       }

getStyles :: LogData -> [SGR]
getStyles log = case logType log of
    LogInfo  -> [SetColor Foreground Vivid Magenta]
    LogError -> [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]

getHandle :: LogData -> Handle
getHandle log = case logType log of
    LogInfo  -> stdout
    LogError -> stderr

logger :: (MonadIO m) => LogData -> m ()
logger log = liftIO $ do 
    let handle = getHandle log
    let styles = getStyles log

    let prefix :: Text -> Text
        prefix name = mconcat ["[", name, "] "]

    let putPrefix  = TextIO.hPutStr   handle . prefix
    let putMessage = TextIO.hPutStrLn handle

    supported <- hSupportsANSIColor handle
    if supported
        then do
            hSetSGR handle styles
            mapM_ putPrefix (logPrefix log)
            hSetSGR handle [Reset]
            putMessage (logMessage log)
        else do
            mapM_ putPrefix (logPrefix log)
            putMessage (logMessage log)
