{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Build.All
( build
) where

import Mewlix.Packager.Config
    ( ProjectConfig(..)
    , ProjectFlag(..)
    )
import Mewlix.Packager.Type (Packager)
import Mewlix.Packager.Build.Modules (compileModules)
import Mewlix.Packager.Build.Templates (generateTemplate)
import Mewlix.Packager.Build.Assets (copyAssets)
import Mewlix.Packager.Log (projectLog)
import Mewlix.Packager.Environment
    ( mewlixFolder
    , buildFolder
    , coreFolder
    , projectFile
    , buildMetafile
    )
import Mewlix.Utils.IO
    ( createDirectory
    , compareFileMods
    , writeFileBytes
    , writeFileText
    )
import Mewlix.Utils.Json (serializeJson)
import Mewlix.Utils.Time (getPosixTimeInSeconds)
import Control.Monad (when, unless)
import System.FilePath ((</>))
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Aeson
    ( ToJSON(..)
    , FromJSON(..)
    , withObject
    , (.=)
    , (.:)
    , object
    )

newtype BuildMetafile = BuildMetaFile { buildTime :: Integer } deriving (Eq, Show)

instance FromJSON BuildMetafile where
    parseJSON = withObject "BuildMetafile" $ \obj -> BuildMetaFile
        <$> (obj .: "timestamp")

instance ToJSON BuildMetafile where
    toJSON metafile = object [ "timestamp" .= buildTime metafile ]

prepare :: Packager ()
prepare = do
    mapM_ (createDirectory False)
        [ mewlixFolder
        , buildFolder ]

build :: ProjectConfig -> Packager ()
build config = do
    projectLog config $ mconcat ["Building project '", projectName config, "'"]
    prepare
    writeTemplate  config
    compileModules config
    copyAssets     config
    writeMetaData  config
    writeReadMe    config

writeTemplate :: ProjectConfig -> Packager ()
writeTemplate config = do
    shouldGenerate <- maybe True (== GT) <$> compareFileMods projectFile buildMetafile
    when shouldGenerate (generateTemplate config)
    timestamp <- getPosixTimeInSeconds

    let meta = BuildMetaFile { buildTime = timestamp }
    writeFileBytes buildMetafile (serializeJson meta)

writeMetaData :: ProjectConfig -> Packager ()
writeMetaData config = do
    let path = coreFolder </> "meta.json"
    let json = object
            [ "title"       .= projectName config
            , "entrypoint"  .= projectEntrypoint config
            , "description" .= projectDescription config ]
    writeFileBytes path (serializeJson json)

writeReadMe :: ProjectConfig -> Packager ()
writeReadMe config = do
    let description = projectDescription config
    let flags = projectFlags config
    let noReadMe = Text.null description || Set.member NoReadMe flags

    unless noReadMe $ do
        let path = buildFolder </> "README.md"
        let contents = Text.concat [ "# ", projectName config, "\n\n", description, "\n" ]
        writeFileText path contents
