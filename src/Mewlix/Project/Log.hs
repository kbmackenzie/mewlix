{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Log
( projectLog
, projectLogError
) where

import Mewlix.Project.Data.Types (ProjectData(..))
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, stdout, stderr)

projectMessage :: ProjectData -> Text -> Text
projectMessage projectData message = mconcat [ "[" , projectName projectData , "] " , message ]

loggerBase :: (MonadIO m) => Handle -> ProjectData -> Text -> m ()
loggerBase handle projectData message = do
    let str = projectMessage projectData message
    liftIO (TextIO.hPutStr handle str)

projectLog :: (MonadIO m) => ProjectData -> Text -> m ()
projectLog = loggerBase stdout

projectLogError :: (MonadIO m) => ProjectData -> Text -> m ()
projectLogError = loggerBase stderr
