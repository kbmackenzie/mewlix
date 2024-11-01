{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Build.All
( build
) where

import Mewlix.Packager.Config
    ( ProjectConfig(..)
    , ProjectFlag(..)
    )
import Mewlix.Packager.Type (Packager, liftEither)
import Mewlix.Packager.Build.Modules (compileModules)
import Mewlix.Packager.Build.Templates (generateTemplate)
import Mewlix.Packager.Build.Assets (copyAssets)
import Mewlix.Packager.Log (projectLog)
import Mewlix.Packager.Environment
    ( mewlixFolder
    , buildFolder
    , coreFolder
    , projectFile
    , buildMetaData
    )
import Mewlix.Utils.IO
    ( createDirectory
    , exists
    , EntryTag(..)
    , getFileModTime
    , readFileBytes
    , writeFileBytes
    , writeFileText
    )
import Mewlix.Utils.Json (serializeJson, parseJson)
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

newtype BuildMetaData = BuildMetaData { buildTime :: Integer } deriving (Eq, Show)

instance FromJSON BuildMetaData where
    parseJSON = withObject "BuildMetaData" $ \obj -> BuildMetaData
        <$> (obj .: "timestamp")

instance ToJSON BuildMetaData where
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
    writeMetaJson  config
    writeReadMe    config

getBuildData :: Packager (Maybe BuildMetaData)
getBuildData = exists File buildMetaData >>= \hasMetafile -> if hasMetafile
    then do
        contents <- readFileBytes buildMetaData
        case parseJson contents of
            (Left _)     -> return Nothing
            (Right data) -> return (Just data)
    else return Nothing

writeTemplate :: ProjectConfig -> Packager ()
writeTemplate config = do
    lastBuild <- fmap buildTime <$> getBuildData
    configMod <- getFileModTime projectFile
    let shouldGenerate = maybe True (configMod >) lastBuild

    when shouldGenerate $
        generateTemplate config

    timestamp <- getPosixTimeInSeconds
    let newBuild = BuildMetaData { buildTime = timestamp }
    writeFileBytes buildMetaData (serializeJson newBuild)

writeMetaJson :: ProjectConfig -> Packager ()
writeMetaJson config = do
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
