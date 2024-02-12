module Mewlix.Project.Folder
( outputFolder
, coreFolder
, moduleFolder
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)

outputFolder :: (MonadIO m) => m FilePath
outputFolder = (</> "output") <$> liftIO getCurrentDirectory

coreFolder :: (MonadIO m) => m FilePath
coreFolder = (</> "core") <$> outputFolder

moduleFolder :: (MonadIO m) => m FilePath
moduleFolder = (</> "project") <$> outputFolder
