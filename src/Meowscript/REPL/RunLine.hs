{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.REPL.RunLine
( takeLine 
, runCommand
, runExpression
) where

import Meowscript.Core.AST
import Meowscript.REPL.Core
import Meowscript.REPL.ParseCommands
import Meowscript.Core.Keys
import Meowscript.Core.Pretty
import Meowscript.Core.Blocks
import Meowscript.Core.RunEvaluator
import Meowscript.Core.Exceptions
import Meowscript.Utils.IO
import Meowscript.Parser.Expr (parseExpr)
import Meowscript.Parser.RunParser (parseSpecial)
import Control.Monad.Reader (ask, asks, liftIO)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Text.Megaparsec as Mega
import Data.IORef (readIORef)
import Data.Functor ((<&>))

replLine :: Parser Line
replLine = Mega.choice
    [ Meta       <$> parseCommand
    , Expression <$> parseExpr   ]

replParse :: Text.Text -> Either Text.Text Line
replParse = parseSpecial replLine

exprAndEnv :: EvalCallback Expr (Prim, Environment)
exprAndEnv x = (,) <$> (evaluate x >>= ensureValue) <*> ask

-- Get pretty-printed primitive from the evaluator itself!
-- I can't pretty-print it afterward, after all. @. @
textAndEnv :: EvalCallback Expr (Text.Text, Environment)
textAndEnv x = (,) <$> (evaluate x >>= ensureValue >>= showMeow) <*> ask

evaluateExpr :: ObjectMap -> Expr -> IO (Either Text.Text (Text.Text, Environment))
evaluateExpr env = runCore (return env) (replTrace . textAndEnv)

replTrace :: Evaluator a -> Evaluator a
replTrace = stackTrace (return "In <repl>.")

---------------------------------------------------------------

takeLine :: ObjectMap -> Text.Text -> REPL (Continue, ObjectMap)
takeLine env line = case replParse line of
    (Left exception) -> liftIO (printStrLn exception) >> return (True, env)
    (Right output) -> case output of
        (Meta com) -> runCommand env com
        (Expression expr) -> runExpression env expr <&> (True,)

runExpression :: ObjectMap -> Expr -> REPL ObjectMap
runExpression env expr = liftIO $ evaluateExpr env expr >>= \case
    (Left exception) -> printError exception >> return env
    (Right (output, env')) -> printStrLn output >> readIORef env'

notCommand :: Text.Text -> Text.Text
notCommand x = Text.concat [ "\"", x, "\" is not a valid command!" ]

runCommand :: ObjectMap -> LineCommand -> REPL (Continue, ObjectMap)
runCommand env com = asks (Map.lookup $ getName com) >>= \case
    Nothing -> (liftIO . printStrLn . notCommand) (getName com) >> return (True, env)
    (Just action) -> liftIO (action com env)
