module Mewlix.Project.IO.ProjectFolder
( projectFolder
, toOutputPath
, makeProjectFolder
, preparePath
) where

import System.FilePath ((</>), takeDirectory, isAbsolute, dropDrive, replaceExtension)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)

projectFolder :: IO FilePath
projectFolder = (</> "mewlix/user/") <$> getCurrentDirectory 

toOutputPath :: FilePath -> IO FilePath
toOutputPath = do
    let handleAbsolute :: FilePath -> FilePath
        handleAbsolute path = if isAbsolute path then dropDrive path else path
    
    let handleExtension :: FilePath -> FilePath
        handleExtension = replaceExtension "js"

    (<$> projectFolder) . flip (</>) . handleExtension . handleAbsolute

preparePath :: FilePath -> IO ()
preparePath = createDirectoryIfMissing True . takeDirectory

makeProjectFolder :: IO ()
makeProjectFolder = createDirectoryIfMissing True =<< projectFolder
