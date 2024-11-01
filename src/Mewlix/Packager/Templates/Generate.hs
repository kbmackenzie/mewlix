{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Templates.Generate
( generateTemplate
, writeReadMe
) where

import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Config (ProjectConfig(..), ProjectMode(..), ProjectFlag(..))
import Mewlix.Packager.Templates.Constants (getTemplate, template)
import Mewlix.Packager.Environment (buildFolder)
import Mewlix.Utils.IO (safelyRun, writeFileText, createDirectory, extractZipDataFile)
import Control.Monad (when, unless)
import Data.Aeson (Value, object, (.=), encode)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Set as Set
import qualified Data.Text as Text

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

writeReadMe :: ProjectConfig -> Packager ()
writeReadMe config = do
    let description = projectDescription config
    let flags = projectFlags config
    let noReadMe = Text.null description || Set.member NoReadMe flags

    unless noReadMe $ do
        let path = buildFolder </> "README.md"
        let contents = Text.concat
                [ "# ", projectName config, "\n\n", description, "\n" ]
        writeFileText path contents
