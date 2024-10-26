module Mewlix.Logger
( LogType(..)
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

data LogType =
      Info
    | Error
    deriving (Show)

splitMessage :: Text -> Maybe (Text, Text)
splitMessage message = do
    (x, _) <- Text.uncons message
    if x == '['
        then do
            index <- succ <$> Text.findIndex (== ']') message
            Just (Text.splitAt index message)
        else Nothing

colorPrefix :: (MonadIO m) => Handle -> [SGR] -> Text -> m ()
colorPrefix handle styles message = case splitMessage message of
    Nothing               -> liftIO $ putLine message
    (Just (header, body)) -> liftIO $ do
        supported <- hSupportsANSIColor handle
        if supported
            then do
                hSetSGR handle styles
                put header
                hSetSGR handle [Reset]
                putLine body
            else putLine message
    where
        put     = TextIO.hPutStr   handle
        putLine = TextIO.hPutStrLn handle

logger :: (MonadIO m) => LogType -> Text -> m ()
logger logtype = case logtype of
    Info    -> colorPrefix stdout [SetColor Foreground Vivid Yellow]
    Error   -> colorPrefix stderr [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity]
