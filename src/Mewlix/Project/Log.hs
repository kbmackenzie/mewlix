{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Log
( projectLog
, projectLogError
) where

import Mewlix.Project.Data.Types (ProjectData(..), ProjectFlag(..))
import Mewlix.Logger (LogType(..), logger);
import Data.Text (Text)
import qualified Data.Set as Set
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (unless)

projectMessage :: ProjectData -> Text -> Text
projectMessage projectData message = mconcat
    [ "[", projectName projectData, "] ", message ]

projectLog :: (MonadIO m) => ProjectData -> Text -> m ()
projectLog projectData message = do
    let quiet = Set.member Quiet (projectFlags projectData)
    unless quiet $
        logger Info (projectMessage projectData message)

projectLogError :: (MonadIO m) => ProjectData -> Text -> m ()
projectLogError = (logger Error .) . projectMessage
