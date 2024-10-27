{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Templates.ReadMe
( createReadme
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Config (ProjectData(..), ProjectFlag(..))
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Utils.IO (writeFileText)
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.FilePath ((</>))
import Control.Monad (unless)

createReadme :: ProjectData -> Packager ()
createReadme projectData = do
    let description = projectDescription projectData
    let flags = projectFlags projectData
    let noReadMe = Text.null description || Set.member NoReadMe flags

    unless noReadMe $ do
        let path = outputFolder </> "README.md"
        let contents = Text.concat
                [ "# ", projectName projectData, "\n\n", description, "\n" ]
        writeFileText path contents
