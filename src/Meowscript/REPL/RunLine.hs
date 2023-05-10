{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Core.RunLine
(
) where

import Meowscript.Core.AST
import Meowscript.Core.Base
import Meowscript.REPL.Commands
import Meowscript.REPL.ParseCommands
import Meowscript.Core.Keys
import Meowscript.Core.Environment
import Meowscript.Core.Pretty
import Meowscript.Core.Blocks
import Meowscript.Core.RunEvaluator
import Meowscript.Parser.RunParser
import Meowscript.Parser.Expr (parseExpr)
import Control.Monad.Reader (ask, asks)
import Text.Megaparsec ((<|>))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Text.Megaparsec as Mega

replLine :: Parser Line
replLine = Mega.choice
    [ Meta       <$> parseCommand
    , Expression <$> parseExpr   ]

exprAndEnv :: EvalCallback Expr (Prim, Environment)
exprAndEnv x = (,) <$> (evaluate x >>= ensureValue) <*> ask

-- Get pretty-printed primitive from the evaluator itself!
-- I can't pretty-print it afterward, after all. @. @
textAndEnv :: EvalCallback Expr (Text.Text, Environment)
textAndEnv x = (,) <$> (evaluate x >>= ensureValue >>= showMeow) <*> ask

readLine :: ObjectMap -> Text.Text -> IO (Text.Text, ObjectMap)
readLine env line = parseLine replLine line >>= \case
    (Left x) -> (return . Left) (x, env)
    (Right x) -> undefined

runExpression :: ObjectMap -> Expr -> IO (Either Text.Text (Text.Text, Environment))
runExpression env = runCore (return env) textAndEnv
