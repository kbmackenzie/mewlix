{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Log
( projectLog
, projectLogError
) where

import Mewlix.Project.Data.Types (ProjectData(..), ProjectFlag(..))
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, stdout, stderr)
import Control.Monad (unless)
import qualified Data.Set as Set

projectMessage :: ProjectData -> Text -> Text
projectMessage projectData message = mconcat [ "[" , projectName projectData , "] " , message ]

loggerBase :: (MonadIO m) => Handle -> ProjectData -> Text -> m ()
loggerBase handle projectData message = do
    let str = projectMessage projectData message
    liftIO (TextIO.hPutStr handle str)

projectLog :: (MonadIO m) => ProjectData -> Text -> m ()
projectLog projectData message = do
    let quiet = Set.member Quiet (projectFlags projectData)
    unless quiet $
        loggerBase stdout projectData message

projectLogError :: (MonadIO m) => ProjectData -> Text -> m ()
projectLogError = loggerBase stderr
