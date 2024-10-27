module Mewlix.Packager.Templates.Create
( createFromTemplate
) where

import Mewlix.Packager.Type (Packager, liftIO)
import Mewlix.Packager.Config.Types (ProjectData(..), ProjectMode(..))
import Mewlix.Packager.Templates.Constants (getTemplate, template)
import Mewlix.Packager.Templates.Node (writePackageData)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Utils.IO (extractZipDataFile)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (when)

createFromTemplate :: ProjectData -> Packager ()
createFromTemplate projectData = do
    let mode = projectMode projectData
    let folder = outputFolder
    let projectTemplate = template mode

    liftIO (createDirectoryIfMissing True folder)
    extractZipDataFile (getTemplate projectTemplate) folder

    when (mode == Node) $
        writePackageData projectData
