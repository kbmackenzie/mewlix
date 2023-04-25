module Meowscript.Core.Evaluate
( Environment
, Evaluator
, ReturnValue
, EnvStack
, Key
) where

import Meowscript.Core.AST
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)

{- Variable context and evaluation monad. -}

type Environment = Map.Map Text.Text Prim
type EnvStack = [Environment]
type Key = Text.Text
type Evaluator a = StateT EnvStack (ExceptT Text.Text IO) a

type ReturnValue a = Maybe a
