{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Statements
(
) where

import Meowscript.Core.AST
import Meowscript.Core.Evaluate
import Meowscript.Core.Expressions
import Meowscript.Core.Messages
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.Reader (ask, local)
import Control.Monad.Except (throwError)
import Control.Monad (void)

runStatement :: [Statement] -> Evaluator (ReturnValue Prim)
runStatement [] = return Nothing
runStatement ((SReturn x):_) = do
    ret <- evaluate x
    return (Just ret)
runStatement (SBreak:_) = return (Just MeowBreak)
runStatement (SContinue:_) = return Nothing
runStatement (x:xs) =
    let run = case x of
            (SExpr y) -> runExprS y
            (SOnlyIf y body) -> runIf y body
            (SIfElse y ifB elseB) -> runIfElse y ifB elseB
            (SWhile y body) -> runWhile y body >> return Nothing
            (SFuncDef name args body) -> runFuncDef name args body
            _ -> return Nothing
    in run >> runStatement xs
    

{- Boolean Condition -}
asCondition :: Expr -> Evaluator Bool
asCondition x = do
    n <- evaluate x
    return (asBool n)


{- Expression Statement -}
runExprS :: Expr -> Evaluator (ReturnValue Prim)

runExprS x = do
    n <- evaluate x
    return (Just n)


{- If Block (Single) -}
runIf :: Expr -> [Statement] -> Evaluator (ReturnValue Prim)

runIf x body = do
    condition <- asCondition x 
    if condition
      then runStatement body
      else return Nothing


{- If Else -}
runIfElse :: Expr -> [Statement] -> [Statement] -> Evaluator (ReturnValue Prim)

runIfElse x ifB elseB = do
    condition <- asCondition x
    if condition
        then runStatement ifB
        else runStatement elseB


{- While Loop -}
{- Notes:
 - A 'True' return value indicates a 'break' statement. -}
runWhile :: Expr -> [Statement] -> Evaluator Bool

runWhile x body = do
    ret <- runStatement body
    condition <- asCondition x
    let isBreak = case ret of
            (Just MeowBreak) -> True
            _ -> False
    if condition && not isBreak
        then runWhile x body
        else return isBreak


{- Function Definition -}

runFuncDef :: Text.Text -> Args -> [Statement] -> Evaluator (ReturnValue Prim)
runFuncDef name args body = do
    env <- ask
    let fn = MeowFunc args body 
    let envFn = const $ Map.insert name fn env
    local envFn $ return Nothing 

