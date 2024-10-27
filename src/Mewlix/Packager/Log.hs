module Mewlix.Packager.Log
( projectLog
, projectLogError
) where

import Mewlix.Packager.Config (ProjectData(..), ProjectFlag(..))
import Mewlix.Logger (LogData(..), LogType(..), logger);
import Data.Text (Text)
import qualified Data.Set as Set
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (unless)

projectLog :: (MonadIO m) => ProjectData -> Text -> m ()
projectLog projectData message = do
    let quiet = Set.member Quiet (projectFlags projectData)
    unless quiet $
        logger LogData
            { logType    = LogInfo
            , logPrefix  = Just (projectName projectData)
            , logMessage = message }

projectLogError :: (MonadIO m) => ProjectData -> Text -> m ()
projectLogError projectData message = logger LogData
    { logType    = LogError
    , logPrefix  = Just (projectName projectData)
    , logMessage = message } 
