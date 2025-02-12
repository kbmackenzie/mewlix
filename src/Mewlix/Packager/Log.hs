module Mewlix.Packager.Log
( buildLog
, initLog
, logMessage
, logError
, logWarning
) where

import Mewlix.Packager.Config (ProjectConfig(..), ProjectFlag(..))
import Mewlix.Logger (LogLevel(..), logger, rainbow, catFace);
import qualified Data.Set as Set
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import System.IO (stdout)
import qualified Data.Text as Text

initLog :: (MonadIO m) => ProjectConfig -> m ()
initLog config = liftIO $ do
    let quiet = Set.member Quiet (projectFlags config)
    unless quiet $ do
        let name = Text.unpack (projectName config)
        putStr "Initializing project "
        rainbow stdout name
        putStr "! "
        catFace stdout
        putChar '\n'

buildLog :: (MonadIO m) => ProjectConfig -> m ()
buildLog config = liftIO $ do
    let quiet = Set.member Quiet (projectFlags config)
    unless quiet $ do
        let name = Text.unpack (projectName config)
        putStr "Building project "
        rainbow stdout name
        putStr "! "
        catFace stdout
        putChar '\n'

logMessage :: (MonadIO m) => ProjectConfig -> String -> m ()
logMessage config message = do
    let quiet = Set.member Quiet (projectFlags config)
    unless quiet (logger LogInfo message)

logError :: (MonadIO m) => String -> m ()
logError = logger LogError

logWarning :: (MonadIO m) => String -> m ()
logWarning = logger LogWarn
