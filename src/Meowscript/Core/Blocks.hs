{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.Blocks
(
) where

import Meowscript.Core.AST
import Data.IORef
import Meowscript.Core.Environment
import Meowscript.Core.Pretty
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Reader (asks, ask, liftIO, local)
import Control.Monad.Except (throwError)
import Data.Functor ((<&>))
import Control.Monad (join)


