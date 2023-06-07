{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.StdFiles
( stdFiles
, asStd
, isStdFile
, readStdFile
) where

import qualified Data.Text as Text
import qualified Data.Set as Set
import Meowscript.Utils.Data

stdFiles :: Set.Set Text.Text
stdFiles = Set.fromList
    [ "std.meows"
    , "io.meows" ]

asStd :: Text.Text -> Text.Text
asStd = Text.append "std/"

isStdFile :: Text.Text -> Bool
isStdFile = flip Set.member stdFiles

readStdFile :: Text.Text -> IO (Either Text.Text Text.Text)
readStdFile = readDataFile . Text.unpack . asStd
