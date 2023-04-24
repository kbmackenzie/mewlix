{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Run
( runEvaluator 
, evaluate
, runStatements
) where

import Meowscript.Core.AST
import Meowscript.Core.Evaluate
import Meowscript.Core.Operations
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
import Control.Monad.Reader (ask, local, runReaderT, liftIO)
import Control.Monad.Except (throwError, runExceptT)
import Control.Monad (void, when)
import Data.Either (Either)

{- Run all statements from root. -}
runEvaluator :: Environment -> Evaluator a -> IO (Either Text.Text a)
runEvaluator env x = runExceptT (runReaderT x env)

{- Evaluating Expressions -}
evaluate :: Expr -> Evaluator Prim
evaluate (EPrim x) = return x
evaluate (EBinop op expA expB) = do
    a <- evaluate expA
    b <- evaluate expB
    binop op a b
evaluate (EUnop op expA) = do
    a <- evaluate expA
    unop op a
evaluate (ECall args name) = do
    fnName <- evaluate name
    case fnName of
        (MeowAtom x) -> funcCall x args
        _ -> throwError "Invalid function name!"
evaluate ERead = do
    x <- liftIO TextIO.getLine
    return $ MeowString x
evaluate (EWrite x) = do
    x' <- evaluate x
    (liftIO . print) x'
    return MeowLonely
-- No need for this last pattern, already matched all options!
--evaluate _ = throwError "Cannot evaluate empty expression!"


funcCall :: Text.Text -> [Expr] -> Evaluator Prim
funcCall name args = do
    env <- ask
    let fn = name `Map.lookup` env
    case fn of
        (Just (MeowFunc params body)) -> do
            when (length params > length args) $ throwError "Too few arguments!"
            let z = zip params args
            void $ addArguments z
            ret <- runStatements body
            local (const env) $ case ret of
                (Just ret') -> return ret'
                _ -> return MeowLonely
        _ -> throwError "Invalid function name!" -- todo: make better error

addArguments :: [(Text.Text, Expr)] -> Evaluator Prim
addArguments [] = return MeowLonely
addArguments ((key, exp'):xs) = do
    env <- ask
    value <- evaluate exp'
    let envFn = const $ Map.insert key value env
    local envFn $ addArguments xs


{- Running Statements -}
runStatements :: [Statement] -> Evaluator (ReturnValue Prim)
runStatements [] = return Nothing
runStatements ((SReturn x):_) = do
    ret <- evaluate x
    ret' <- lookUpSafe ret
    return (Just ret')
runStatements (SBreak:_) = return (Just MeowBreak)
runStatements (SContinue:_) = return Nothing
runStatements ((SExpr x):xs) = runExprS x >> runStatements xs
runStatements ((SFuncDef name args body):xs) = runFuncDef name args body >> runStatements xs
runStatements (x:xs) =
    let run = case x of
            (SOnlyIf y body) -> runIf y body
            (SIfElse y ifB elseB) -> runIfElse y ifB elseB
            (SWhile y body) -> runWhile y body >> return Nothing
            _ -> return Nothing
    in do
        env <- ask
        void run
        local (const env) $ runStatements xs
    

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
      then runStatements body
      else return Nothing


{- If Else -}
runIfElse :: Expr -> [Statement] -> [Statement] -> Evaluator (ReturnValue Prim)

runIfElse x ifB elseB = do
    condition <- asCondition x
    if condition
        then runStatements ifB
        else runStatements elseB


{- While Loop -}
{- Notes:
 - A 'True' return value indicates a 'break' statement. -}
runWhile :: Expr -> [Statement] -> Evaluator Bool

runWhile x body = do
    ret <- runStatements body
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
