module Meowscript.Evaluate.Evaluator
(
) where

import Meowscript.Evaluate.Environment
import Meowscript.Evaluate.MeowThrower
import qualified Data.Text as Text

data EvaluatorState p = EvaluatorState
    { evaluatorCtx  :: Context p
    , moduleInfo    :: ModuleInfo
    , evaluatorMeta :: EvaluatorMeta p }

data ModuleInfo = ModuleInfo
data EvaluatorMeta p = EvaluatorMeta
