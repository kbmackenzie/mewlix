module Mewlix.Packager.Templates.Create
( createFromTemplate
) where

import Mewlix.Packager.Maker (PackageMaker, liftIO)
import Mewlix.Packager.Data.Types (ProjectMode(..))
import Mewlix.Packager.Templates.Constants (getTemplate, template)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Utils.FileIO (extractDataFile)
import System.Directory (createDirectoryIfMissing)

createFromTemplate :: ProjectMode -> PackageMaker ()
createFromTemplate mode = do
    let folder = outputFolder
    let projectTemplate = template mode

    liftIO (createDirectoryIfMissing True folder)
    extractDataFile (getTemplate projectTemplate) folder
