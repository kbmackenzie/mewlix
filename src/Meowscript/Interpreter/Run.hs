module Meowscript.Interpreter.Run
( interpret
, runModule
, makeState
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Evaluate.Evaluator
import Meowscript.Evaluate.State
import Meowscript.Evaluate.Environment
import Meowscript.Evaluate.Exception
import Meowscript.Interpreter.Module
import Meowscript.Interpreter.Interpret
import Meowscript.Parser.AST
import Lens.Micro.Platform (set)

{- Run Interpreter -}
--------------------------------------------------------------------
interpret :: Evaluator p a -> EvaluatorState p -> IO (EvaluatorState p, Either CatException a)
interpret = runEvaluator

runModule :: FilePath -> Evaluator MeowAtom ReturnValue
runModule path = do
    (resolved, mainModule) <- readModule path
    setState (set (moduleInfoL.modulePathL) resolved)
    statement (getModule mainModule)

{- Evaluator State -}
--------------------------------------------------------------------
makeState :: (MonadIO m) => FilePath -> Bool -> m (EvaluatorState p)
makeState path isMain = do
    ctx     <- initContext 
    meta    <- emptyMeta
    let info = ModuleInfo {
        modulePath   = path,
        moduleIsMain = isMain
    }
    return EvaluatorState {
        evaluatorCtx  = ctx,
        moduleInfo    = info,
        evaluatorMeta = meta
    }
