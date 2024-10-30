module Mewlix.Packager.Templates.Create
( createFromTemplate
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Config (ProjectData(..), ProjectMode(..))
import Mewlix.Packager.Templates.Constants (getTemplate, template)
import Mewlix.Packager.Templates.Node (writePackageData)
import Mewlix.Packager.Folder (buildFolder)
import Mewlix.Utils.IO (createDirectory, extractZipDataFile)
import Control.Monad (when)

createFromTemplate :: ProjectData -> Packager ()
createFromTemplate projectData = do
    let mode = projectMode projectData
    let folder = buildFolder
    let projectTemplate = template mode

    createDirectory True folder
    extractZipDataFile (getTemplate projectTemplate) folder

    when (mode == Node) $
        writePackageData projectData
