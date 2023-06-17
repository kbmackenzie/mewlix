{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.StdFiles
( stdFiles
, asStd
, isStdFile
, readStdFile
) where

import qualified Data.Text as Text
import qualified Data.Set as Set
import Meowscript.Utils.Types
import Meowscript.Utils.Data

stdFiles :: Set.Set FilePathT
stdFiles = Set.fromList
    [ "std.meows"
    , "io.meows"
    , "time.meows"
    , "assert.meows"
    , "cattree.meows"
    , "hashmap.meows"
    , "hashset.meows"
    , "exception.meows" ]

asStd :: FilePathT -> FilePathT
asStd = Text.append "std/"

isStdFile :: FilePathT -> Bool
isStdFile = flip Set.member stdFiles

readStdFile :: FilePathT -> IO (Either Text.Text Text.Text)
readStdFile = readDataFile . Text.unpack . asStd
