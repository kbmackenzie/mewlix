module Meowscript.Core.Operations
(
) where

import Meowscript.Core.AST
import Data.IORef
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.Reader (asks, ask, liftIO, local)
import Control.Monad.Except (throwError)
import Data.Functor ((<&>))
