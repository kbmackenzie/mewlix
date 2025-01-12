module Mewlix.Packager.Log
( buildLog
, initLog
, logMessage
, logError
, logWarning
) where

import Mewlix.Packager.Config (ProjectConfig(..), ProjectFlag(..))
import Mewlix.Logger (LogLevel(..), logger, rainbow);
import Data.Text (Text)
import qualified Data.Set as Set
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import System.IO (stdout)
import qualified Data.Text as Text

initLog :: (MonadIO m) => ProjectConfig -> m ()
initLog config = liftIO $ do
    let name = Text.unpack (projectName config)
    putStr "Initializing project "
    rainbow stdout name
    putStrLn "! 🐱"

buildLog :: (MonadIO m) => ProjectConfig -> m ()
buildLog config = liftIO $ do
    let name = Text.unpack (projectName config)
    putStr "Building project "
    rainbow stdout name
    putStrLn "! 🐱"

logMessage :: (MonadIO m) => ProjectConfig -> Text -> m ()
logMessage config message = do
    let quiet = Set.member Quiet (projectFlags config)
    unless quiet (logger LogInfo message)

logError :: (MonadIO m) => Text -> m ()
logError = logger LogError

logWarning :: (MonadIO m) => Text -> m ()
logWarning = logger LogWarn
