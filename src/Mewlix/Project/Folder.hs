module Mewlix.Project.Folder
( outputFolder
, coreFolder
, moduleFolder
) where

import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)

outputFolder :: IO FilePath
outputFolder = (</> "output") <$> getCurrentDirectory

coreFolder :: IO FilePath
coreFolder = (</> "core") <$> outputFolder

moduleFolder :: IO FilePath
moduleFolder = (</> "project") <$> outputFolder
