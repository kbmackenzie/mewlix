module Meowscript.Utils.Data
( readDataFile
) where

import Meowscript.Utils.IO
import Paths_Meowscript
import qualified Data.Text as Text

readDataFile :: FilePath -> IO (Either Text.Text Text.Text)
{-# INLINABLE readDataFile #-}
readDataFile path = getDataFileName path >>= safeReadFile
