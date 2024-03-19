module Mewlix.Project.Folder
( outputFolder
, coreFolder
, moduleFolder
, projectFile
) where

import System.FilePath ((</>))

outputFolder :: FilePath
outputFolder = "output"

coreFolder :: FilePath
coreFolder = outputFolder </> "core"

moduleFolder :: FilePath
moduleFolder = outputFolder </> "yarnballs"

projectFile :: FilePath
projectFile = "./mewlix.yaml"
