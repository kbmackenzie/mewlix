{-# LANGUAGE TupleSections #-}

module Mewlix.REPL.Expr
(
) where

import Mewlix.Abstract.Meow
import Mewlix.Abstract.State
import Mewlix.Abstract.Meowable
import Mewlix.Abstract.Prettify
import Mewlix.Parser.AST (Expr)
import Mewlix.Interpreter.Interpret
import Mewlix.Interpreter.API
import Mewlix.REPL.Core
import Data.Text (Text)
import Control.Monad ((>=>))

type REPLOutput = (Text, EvaluatorState MeowPrim)

replExpression :: Expr -> REPL (Either CatException REPLOutput)
replExpression expr state = do
    let run :: Evaluator REPLOutput
        run = (,) <$> (expression >=> prettyMeow) expr <*> ask
    interpret state run

replImport :: FilePath -> REPL (Either CatException MewlixState)
replImport path state = do
    let run :: Evaluator MewlixState
        run = runAsImport path Nothing >> ask
    interpret state run
