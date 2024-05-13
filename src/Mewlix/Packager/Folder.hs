module Mewlix.Packager.Folder
( outputFolder
, coreFolder
, moduleFolder
, projectFile
) where

import System.FilePath ((</>))

outputFolder :: FilePath
outputFolder = "build"

coreFolder :: FilePath
coreFolder = outputFolder </> "core"

moduleFolder :: FilePath
moduleFolder = outputFolder </> "yarnball"

projectFile :: FilePath
projectFile = "./mewlix.yaml"
