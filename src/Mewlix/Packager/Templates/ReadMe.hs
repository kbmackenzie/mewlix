{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Templates.ReadMe
( createReadme
) where

import Mewlix.Packager.Data.Types (ProjectData(..), ProjectFlag(..))
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Utils.FileIO (writeText)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.FilePath ((</>))
import Control.Monad (unless)

createReadme :: (MonadIO m) => ProjectData -> m ()
createReadme projectData = do
    let description = projectDescription projectData
    let flags = projectFlags projectData
    let noReadMe = Text.null description || Set.member NoReadMe flags

    unless noReadMe $ do
        let path = outputFolder </> "README.md"
        let contents = Text.concat
                [ "# ", projectName projectData, "\n\n", description, "\n" ]
        liftIO (writeText path contents)
