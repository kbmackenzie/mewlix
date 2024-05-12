module Mewlix.Packager.Templates.Create
( createFromTemplate
) where

import Mewlix.Packager.Maker (ProjectMaker, ProjectContext(..), asks, liftIO)
import Mewlix.Packager.Data.Types (ProjectMode(..))
import Mewlix.Packager.Templates.Constants (getTemplate, template)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Utils.FileIO (extractDataFile)
import System.Directory (createDirectoryIfMissing)

createFromTemplate :: ProjectMode -> ProjectMaker ()
createFromTemplate mode = do
    language <- asks projectLanguage
    let folder = outputFolder
    let projectTemplate = template language mode

    liftIO (createDirectoryIfMissing True folder)
    extractDataFile (getTemplate projectTemplate) folder
