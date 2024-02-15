{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Templates.ReadMe
( createReadme
) where

import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Utils.FileIO (writeFileT)
import qualified Data.Text as Text
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Control.Monad (unless)

createReadme :: (MonadIO m) => ProjectData -> m ()
createReadme projectData = do
    let path = outputFolder </> "README.md"
    let contents = Text.unlines
            [ "# " <> projectName projectData 
            , projectDescription projectData  ]

    exists <- liftIO (doesFileExist path)
    unless exists $
        liftIO (writeFileT path contents)
