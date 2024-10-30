{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Templates.Generate
( generateTemplate
, writeReadMe
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Config (ProjectData(..), ProjectMode(..), ProjectFlag(..))
import Mewlix.Packager.Templates.Constants (getTemplate, template)
import Mewlix.Packager.Folder (buildFolder)
import Mewlix.Utils.IO (safelyRun, writeFileText, createDirectory, extractZipDataFile)
import Control.Monad (when, unless)
import Data.Aeson (Value, object, (.=), encode)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Set as Set
import qualified Data.Text as Text

generateTemplate :: ProjectData -> Packager ()
generateTemplate projectData = do
    let mode = projectMode projectData
    let folder = buildFolder
    let projectTemplate = template mode

    createDirectory True folder
    extractZipDataFile (getTemplate projectTemplate) folder

    when (mode == Node) $
        writePackageJson projectData

packageJson :: ProjectData -> Value
packageJson projectData = object
    [ "name"        .= projectName projectData
    , "version"     .= ("1.0.0" :: String)
    , "description" .= projectDescription projectData
    , "type"        .= ("module" :: String)           ]

writePackageJson :: ProjectData -> Packager ()
writePackageJson projectData = do
    let package = packageJson projectData
    let path = buildFolder </> "package.json"

    let action = ByteString.writeFile path (encode package)
    safelyRun action "couldn't write package data"

writeReadMe :: ProjectData -> Packager ()
writeReadMe projectData = do
    let description = projectDescription projectData
    let flags = projectFlags projectData
    let noReadMe = Text.null description || Set.member NoReadMe flags

    unless noReadMe $ do
        let path = buildFolder </> "README.md"
        let contents = Text.concat
                [ "# ", projectName projectData, "\n\n", description, "\n" ]
        writeFileText path contents
