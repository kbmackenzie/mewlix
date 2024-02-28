module Mewlix.Logger
( LogType(..)
, logger
) where

import Mewlix.Utils.Logging (hPutUtil)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (Handle, stdout, stderr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Console.ANSI
    ( SGR(..)
    , Color(..)
    , ConsoleLayer(..)
    , ColorIntensity(..)
    , setSGR
    , hSupportsANSIColor
    )

data LogType =
      Info
    | Warning
    | Error
    deriving (Show)

splitMessage :: Text -> Maybe (Text, Text)
splitMessage message = do
    (x, _) <- Text.uncons message
    if x == '['
        then do
            i <- Text.findIndex (== ']') message
            Just (Text.splitAt i message)
        else Nothing

colorPrint :: (MonadIO m) => Handle -> [SGR] -> Text -> m ()
colorPrint handle sgr message = case splitMessage message of
    Nothing               -> put message
    (Just (header, body)) -> do
        supported <- liftIO $ hSupportsANSIColor handle
        if supported
            then do
                liftIO $ setSGR sgr
                put header
                liftIO $ setSGR [Reset]
                put body
            else put message
    where put = hPutUtil handle

logger :: (MonadIO m) => LogType -> Text -> m ()
logger logtype = case logtype of
    Info    -> colorPrint stdout []
    Warning -> colorPrint stdout [SetColor Foreground Vivid Yellow]
    Error   -> colorPrint stderr [SetColor Foreground Vivid Magenta]
