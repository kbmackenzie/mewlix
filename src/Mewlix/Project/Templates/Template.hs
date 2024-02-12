{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Templates.Template
( createFromTemplate
) where

import Mewlix.Project.Maker
    ( ProjectMaker
    , Language(..)
    , ProjectContext(..)
    , liftIO
    , throwError
    , asks
    )
import Mewlix.Utils.FileIO (copyDataFile)
import Mewlix.Project.Data.Types (ProjectMode(..))
import System.FilePath ((</>), takeFileName)
import System.Directory (createDirectoryIfMissing)

data Template = Template
    { templateCore :: [FilePath]
    , templateBody :: [FilePath] }
    deriving (Show)

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

    liftIO $ mapM_ (createDirectoryIfMissing False)
        [ "output"
        , "output/mewlix"
        , "output/mewlix/core"
        , "output/mewlix/user" ]

    let copyData :: FilePath -> FilePath -> ProjectMaker ()
        copyData targetFolder dataPath = do
            let targetPath = targetFolder </> takeFileName dataPath
            copyDataFile dataPath targetPath

    let copyAllData :: FilePath -> [FilePath] -> ProjectMaker ()
        copyAllData targetFolder = mapM_ (copyData targetFolder)

    copyAllData "output/mewlix/core" (templateCore template)
    copyAllData "output/mewlix"      (templateBody template)
