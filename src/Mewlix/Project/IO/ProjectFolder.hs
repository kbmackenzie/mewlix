module Mewlix.Project.IO.ProjectFolder
( projectFolder
, toOutputPath
, makeProjectFolder
, preparePath
) where

import System.FilePath ((</>), takeDirectory)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)

projectFolder :: IO FilePath
projectFolder = (</> "mewlix/user/") <$> getCurrentDirectory 

toOutputPath :: FilePath -> IO FilePath
toOutputPath = (<$> getCurrentDirectory) . flip (</>)

preparePath :: FilePath -> IO ()
preparePath = createDirectoryIfMissing True . takeDirectory

makeProjectFolder :: IO ()
makeProjectFolder = createDirectoryIfMissing True =<< projectFolder
