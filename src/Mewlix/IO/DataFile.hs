module Mewlix.IO.DataFile
( readDataFile
) where

import Mewlix.IO.File (safeReadFile)
import Paths_Mewlix
import qualified Data.Text as Text

readDataFile :: FilePath -> IO (Either Text.Text Text.Text)
{-# INLINABLE readDataFile #-}
readDataFile path = getDataFileName path >>= safeReadFile
