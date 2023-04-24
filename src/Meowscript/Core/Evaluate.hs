module Meowscript.Core.Evaluate
( Environment
, Evaluator
) where

import Meowscript.Core.AST
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)

{- Variable context and evaluation monad. -}

type Environment = Map.Map Text.Text Prim
type Evaluator a = ReaderT Environment (ExceptT Text.Text IO) a
