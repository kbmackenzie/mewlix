module Mewlix.Packager.Templates.Create
( createFromTemplate
) where

import Mewlix.Packager.Maker (PackageMaker, liftIO)
import Mewlix.Packager.Data.Types (ProjectData(..), ProjectMode(..))
import Mewlix.Packager.Templates.Constants (getTemplate, template)
import Mewlix.Packager.Templates.Node (writePackageData)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Utils.FileIO (extractDataFile)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (when)

createFromTemplate :: ProjectData -> PackageMaker ()
createFromTemplate projectData = do
    let mode = projectMode projectData
    let folder = outputFolder
    let projectTemplate = template mode

    liftIO (createDirectoryIfMissing True folder)
    extractDataFile (getTemplate projectTemplate) folder

    when (mode == Library) $
        writePackageData projectData
