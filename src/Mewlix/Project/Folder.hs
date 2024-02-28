module Mewlix.Project.Folder
( outputFolder
, coreFolder
, moduleFolder
) where

import System.FilePath ((</>))

outputFolder :: FilePath
outputFolder = "output"

coreFolder :: FilePath
coreFolder = outputFolder </> "core"

moduleFolder :: FilePath
moduleFolder = outputFolder </> "yarn"
