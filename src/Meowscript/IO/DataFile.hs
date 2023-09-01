module Meowscript.IO.DataFile
( readDataFile
) where

import Meowscript.IO.File (safeReadFile)
import Paths_Meowscript
import qualified Data.Text as Text

readDataFile :: FilePath -> IO (Either Text.Text Text.Text)
{-# INLINABLE readDataFile #-}
readDataFile path = getDataFileName path >>= safeReadFile
