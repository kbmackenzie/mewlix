{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.StdFiles
( stdFiles
, asStd
, isStdFile
, readStdFile
) where

import qualified Data.Text as Text
import Meowscript.Utils.Data

stdFiles :: [Text.Text]
stdFiles = 
    [ "std.meows"
    , "io.meows" ]

asStd :: Text.Text -> Text.Text
asStd = Text.append "std/"

isStdFile :: Text.Text -> Bool
isStdFile = flip elem stdFiles

readStdFile :: Text.Text -> IO (Either Text.Text Text.Text)
readStdFile = readDataFile . Text.unpack . asStd
