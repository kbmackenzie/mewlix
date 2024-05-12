{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Project.Templates.ReadMe
( createReadme
) where

import Mewlix.Project.Data.Types (ProjectData(..), ProjectFlag(..))
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Utils.FileIO (writeText)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.FilePath ((</>))
import Control.Monad (unless)

createReadme :: (MonadIO m) => ProjectData -> m ()
createReadme projectData = do
    let noReadMe = Set.member NoReadMe (projectFlags projectData)
    unless noReadMe $ do
        let path = outputFolder </> "README.md"
        let contents = Text.unlines
                [ "# " <> projectName projectData 
                , mempty
                , projectDescription projectData  ]
        liftIO (writeText path contents)
