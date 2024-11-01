module Mewlix.Packager.Environment
( mewlixFolder
, buildFolder
, coreFolder
, moduleFolder
, projectFile
, buildMetaData
) where

import System.FilePath ((</>))

mewlixFolder :: FilePath
mewlixFolder = ".mewlix"

buildFolder :: FilePath
buildFolder = mewlixFolder </> "build"

coreFolder :: FilePath
coreFolder = buildFolder </> "core"

moduleFolder :: FilePath
moduleFolder = buildFolder </> "yarnball"

projectFile :: FilePath
projectFile = "./mewlix.yaml"

buildMetaData :: FilePath
buildMetaData = buildFolder </> "mewlix-build.json"
