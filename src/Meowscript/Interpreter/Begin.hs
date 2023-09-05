{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Interpreter.Begin
( interpret
, runFile
, begin
, runImport
, librarySet
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Evaluate.Evaluator
import Meowscript.Evaluate.State
import Meowscript.Evaluate.Environment
import Meowscript.Evaluate.Exception
import Meowscript.Interpreter.Module
import Meowscript.Interpreter.Import
import Meowscript.Interpreter.Interpret
import Meowscript.Interpreter.Exceptions
import Meowscript.Data.Key (Key)
import qualified Data.Text as Text
import qualified Meowscript.Data.Stack as Stack
import Meowscript.Parser.AST
import Lens.Micro.Platform ((.~))

type InterpreterOutput a = (EvaluatorState MeowAtom, Either CatException a)
type StateTransform = EvaluatorState MeowAtom -> EvaluatorState MeowAtom

{- Run Interpreter -}
--------------------------------------------------------------------
interpret :: Evaluator p a -> EvaluatorState p -> IO (EvaluatorState p, Either CatException a)
interpret = runEvaluator

{- This function:
 - 1. Parses a file into a Module.
 - 2. Creates an empty state.
 - 3. Applies transformations to that empty state.
 - 4. Runs the module in the generated state.
 - 5. Runs the evaluator, extracting the EvaluatorState and ReturnValue. -}
begin :: FilePath -> Evaluator MeowAtom a -> [StateTransform] -> IO (InterpreterOutput a)
begin path action ts = do
    state <- initState path True
    let transformed = foldr ($) state ts
    interpret action transformed
   
-- Parse a file into a Module and run it.
runFile :: FilePath -> Evaluator MeowAtom ReturnValue
runFile path = do
    (resolved, mainModule) <- readModule path
    liftIO (print (getModule mainModule))
    setState $ moduleInfoL.modulePathL .~ resolved
    runModule mainModule

-- Run a module. (This function affects context.)
runModule :: Module -> Evaluator MeowAtom ReturnValue
runModule (Module moduleBlock) = do
    --let (imports, rest) = Stack.partition isImport moduleBlock
    --mapM_ (uncurry runImport . fromImport) imports
    -- Note to self: Stack.partition reverses the stack. @-----@ fix that.
    statement moduleBlock

-- Run import module in sandbox and then import its global environment.
runImport :: FilePath -> Maybe Key -> Evaluator MeowAtom ()
runImport path key = do
    cleanState <- getState >>= cleanContext
    meowImport key path runFile cleanState

librarySet :: Evaluator MeowAtom ()
librarySet = do
    libs <- joinLibraries <$> askState evaluatorLibs
    global <- askState (globalEnv . evaluatorCtx)
    modifyRef (<> libs) global

{- Presets -}
-----------------------------------------------------------------------------------------

