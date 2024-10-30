module Mewlix.Packager.Log
( projectLog
, projectLogError
) where

import Mewlix.Packager.Config (ProjectConfig(..), ProjectFlag(..))
import Mewlix.Logger (LogData(..), LogType(..), logger);
import Data.Text (Text)
import qualified Data.Set as Set
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (unless)

projectLog :: (MonadIO m) => ProjectConfig -> Text -> m ()
projectLog config message = do
    let quiet = Set.member Quiet (projectFlags config)
    unless quiet $
        logger LogData
            { logType    = LogInfo
            , logPrefix  = Just (projectName config)
            , logMessage = message }

projectLogError :: (MonadIO m) => ProjectConfig -> Text -> m ()
projectLogError config message = logger LogData
    { logType    = LogError
    , logPrefix  = Just (projectName config)
    , logMessage = message } 
