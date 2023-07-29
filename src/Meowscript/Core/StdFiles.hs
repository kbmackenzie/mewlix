{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.StdFiles
( stdFiles
, asStd
, isStdFile
, readStdFile
) where

import Meowscript.Utils.Types
import Meowscript.Utils.Data
import qualified Data.Text as Text
import qualified Data.Set as Set

stdFiles :: Set.Set FilePathT
{-# INLINE stdFiles #-}
stdFiles = Set.fromList
    [ "std.meows"
    , "io.meows"
    , "time.meows"
    , "assert.meows"
    , "cat_tree.meows"
    , "hashmap.meows"
    , "hashset.meows"
    , "numbers.meows"
    , "exception.meows" ]

asStd :: FilePathT -> FilePathT
{-# INLINE asStd #-}
asStd = Text.append "std/"

isStdFile :: FilePathT -> Bool
{-# INLINE isStdFile #-}
isStdFile = flip Set.member stdFiles

readStdFile :: FilePathT -> IO (Either Text.Text Text.Text)
{-# INLINE readStdFile #-}
readStdFile = readDataFile . Text.unpack . asStd
