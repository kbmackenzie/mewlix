{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Template
( createFromTemplate
) where

import Mewlix.Project.Make
    ( ProjectMaker
    , Language(..)
    , ProjectContext(..)
    , liftIO
    , throwError
    , asks
    )
import qualified Mewlix.Utils.IO as FileIO
import Mewlix.Project.Mode (ProjectMode(..))
import System.FilePath ((</>), takeFileName)
import System.Directory (createDirectory)

data Template = Template
    { templateCore :: [FilePath]
    , templateBody :: [FilePath] }
    deriving (Show)

copyDataFile :: FilePath -> FilePath -> ProjectMaker ()
copyDataFile dataPath targetPath = do
    contents <- FileIO.readDataFile dataPath >>= \case
        (Left err)  -> throwError (show err)
        (Right dat) -> return dat
    FileIO.writeFile targetPath contents

templateFile :: FilePath -> FilePath
templateFile = ("template/mewlix-base" </>)

templateJS :: ProjectMode -> Template
templateJS mode = case mode of
    Console -> Template {
        templateCore = [
            templateFile "mewlix.js",
            templateFile "mewlix-console/console.js"
        ],
        templateBody = [
            templateFile "mewlix-console/index.html",
            templateFile "mewlix-console/style.css"
        ]
    }

    Graphic -> Template {
        templateCore = [
            templateFile "mewlix.js",
            templateFile "mewlix-graphic/graphic.js"
        ],
        templateBody = [
            templateFile "mewlix-graphic/index.html",
            templateFile "mewlix-graphic/style.css"
        ]
    }

    Library -> Template {
        templateCore = [ templateFile "mewlix.js" ],
        templateBody = []
    }

getTemplate :: Language -> ProjectMode -> Template
getTemplate Javascript = templateJS

createFromTemplate :: ProjectMode -> ProjectMaker ()
createFromTemplate mode = do
    language <- asks projectLanguage
    let template = getTemplate language mode

    liftIO $ mapM_ createDirectory
        [ "mewlix-output"
        , "mewlix-output/mewlix"
        , "mewlix-output/mewlix/core"
        , "mewlix-output/mewlix/user" ]

    let copyData :: FilePath -> FilePath -> ProjectMaker ()
        copyData targetFolder dataPath = do
            let targetPath = targetFolder </> takeFileName dataPath
            copyDataFile dataPath targetPath

    let copyAllData :: FilePath -> [FilePath] -> ProjectMaker ()
        copyAllData targetFolder = mapM_ (copyData targetFolder)

    copyAllData "mewlix-output/mewlix/core" (templateCore template)
    copyAllData "mewlix-output/mewlix"      (templateBody template)
