{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Packager.Build.Templates
( generateTemplate
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Config (ProjectConfig(..), ProjectMode(..))
import Mewlix.Packager.Environment (buildFolder)
import Mewlix.Utils.IO (safelyRun, createDirectory, extractZipDataFile)
import Control.Monad (when)
import Data.Aeson (Value, object, (.=), encode)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as ByteString

newtype Template = Template { getTemplate :: FilePath }
    deriving (Eq, Show)

templatePath :: FilePath -> FilePath
templatePath = ("static/templates/" </>)

template :: ProjectMode -> Template
template = \case
    Console -> Template { getTemplate = templatePath "console.zip" }
    Graphic -> Template { getTemplate = templatePath "graphic.zip" }
    Node    -> Template { getTemplate = templatePath "node.zip"    }
    Blank   -> Template { getTemplate = templatePath "blank.zip"   }

generateTemplate :: ProjectConfig -> Packager ()
generateTemplate config = do
    let mode = projectMode config
    let folder = buildFolder
    let projectTemplate = template mode

    createDirectory True folder
    extractZipDataFile (getTemplate projectTemplate) folder

    when (mode == Node) $
        writePackageJson config

packageJson :: ProjectConfig -> Value
packageJson config = object
    [ "name"        .= projectName config
    , "version"     .= ("1.0.0" :: String)
    , "description" .= projectDescription config
    , "type"        .= ("module" :: String)           ]

writePackageJson :: ProjectConfig -> Packager ()
writePackageJson config = do
    let package = packageJson config
    let path = buildFolder </> "package.json"

    let action = ByteString.writeFile path (encode package)
    safelyRun action "couldn't write package data"
