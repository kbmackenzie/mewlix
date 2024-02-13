module Mewlix.Project.Templates.Create
( createFromTemplate
) where

import Mewlix.Project.Maker (ProjectMaker, ProjectContext(..), asks, liftIO)
import Mewlix.Project.Data.Types (ProjectMode(..))
import Mewlix.Project.Templates.Constants (getTemplate, template)
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Utils.FileIO (extractDataFile)
import System.Directory (createDirectoryIfMissing)

createFromTemplate :: ProjectMode -> ProjectMaker ()
createFromTemplate mode = do
    language <- asks projectLanguage
    let folder = outputFolder
    let projectTemplate = template language mode

    liftIO (createDirectoryIfMissing True folder)
    extractDataFile (getTemplate projectTemplate) folder
