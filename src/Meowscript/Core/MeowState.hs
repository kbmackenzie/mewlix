module Meowscript.Core.MeowState
( meowState
, meowNewPath
) where

import Meowscript.Core.AST
import Meowscript.Utils.Types
import Meowscript.Core.StdFiles
import qualified Data.Text as Text

meowState :: FilePathT -> [Text.Text] -> IO ObjectMap -> MeowState
meowState path args lib = MeowState
    { meowArgs   = args
    , meowLib    = lib
    , meowStd    = stdFiles
    , meowPath   = path
    , meowSocket = Nothing }

meowNewPath :: MeowState -> FilePathT -> MeowState
meowNewPath (MeowState args lib std _ socket) = flip (MeowState args lib std) socket
