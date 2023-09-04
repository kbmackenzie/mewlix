module Meowscript.Interpreter.Begin
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
import qualified Meowscript.Data.Stack as Stack
import Meowscript.Parser.AST
import Lens.Micro.Platform (set)

{- Run Interpreter -}
--------------------------------------------------------------------
interpret :: Evaluator p a -> EvaluatorState p -> IO (EvaluatorState p, Either CatException a)
interpret = runEvaluator

runModule :: FilePath -> Evaluator MeowAtom ReturnValue
runModule path = do
    (resolved, mainModule) <- readModule path
    let moduleBlock = getModule mainModule
    setState (set (moduleInfoL.modulePathL) resolved)

    let isImport :: Statement -> Bool
        isImport (StmtImport _ _) = True
        isImport _                = False

    let runImport :: Statement -> Evaluator MeowAtom ()
        runImport _ = undefined

    let (imports, rest) = Stack.partition isImport moduleBlock
    mapM_ runImport (Stack.toList imports)

    statement rest

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
