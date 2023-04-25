{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Run
( runEvaluator 
, evaluate
, runStatements
) where

import Meowscript.Core.AST
import Meowscript.Core.Evaluate
import Meowscript.Core.Operations
import Meowscript.Core.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
import Control.Monad.State (get, put, liftIO, runStateT)
import Control.Monad.Except (throwError, runExceptT)
import Control.Monad (void, when)
import Data.Either (Either)

{- Run all statements from root. -}
runEvaluator :: EnvStack -> Evaluator a -> IO (Either Text.Text (a, EnvStack))
runEvaluator env x = runExceptT (runStateT x env)

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
    x'' <- ensureValue x'
    (liftIO . print) x''
    return MeowLonely


{- Ensures a value will be passed instead of an atom. -}
ensureValue :: Prim -> Evaluator Prim
ensureValue (MeowAtom x) = lookUpVar x
ensureValue x = return x


{- Function Call -}
funcCall :: Text.Text -> [Expr] -> Evaluator Prim
funcCall name args = do
    x <- keyExists name
    if not x then
        throwError (Text.concat ["Function doesn't exist! :", name])
    else do
        (MeowFunc params body) <- lookUpVar name
        when (length params > length args) $ throwError "Too few arguments!"
        let z = zip params args
        stackPush
        funcArgs z
        ret <- runStatements body
        stackPop
        return ret

{- Add function arguments to the environment. -}
funcArgs :: [(Key, Expr)] -> Evaluator ()
funcArgs [] = return ()
funcArgs ((key, expr):xs) = do
    value <- evaluate expr
    value' <- ensureValue value
    addToTop key value'
    funcArgs xs



{- Running Statements -}
runStatements :: [Statement] -> Evaluator Prim
runStatements [] = return MeowVoid

runStatements ((SReturn x):_) = do
    ret <- evaluate x
    ensureValue ret

runStatements (SBreak:_) = return MeowBreak
runStatements (SContinue:_) = return MeowVoid

runStatements ((SExpr x):xs) =
    runExprS x >> runStatements xs

runStatements ((SFuncDef name args body):xs) =
    runFuncDef name args body >> runStatements xs

runStatements (x:xs) = do
    stackPush
    ret <- run
    stackPop
    if ret /= MeowVoid
        then return ret
        else runStatements xs
    where run = case x of
            (SOnlyIf y body) -> runIf y body
            (SIfElse y ifB elseB) -> runIfElse y ifB elseB
            (SWhile y body) -> runWhile y body
            _ -> return MeowVoid


{- Boolean Condition -}
asCondition :: Expr -> Evaluator Bool
asCondition x = do
    n <- evaluate x
    n' <- ensureValue n
    return (asBool n')

{- Expression Statement -}
runExprS :: Expr -> Evaluator Prim
runExprS x = do
    evaluate x

{- If Block (Single) -}
runIf :: Expr -> [Statement] -> Evaluator Prim
runIf x body = do
    condition <- asCondition x 
    if condition
      then runStatements body
      else return MeowVoid

{- If Else -}
runIfElse :: Expr -> [Statement] -> [Statement] -> Evaluator Prim
runIfElse x ifB elseB = do
    condition <- asCondition x
    if condition
        then runStatements ifB
        else runStatements elseB

{- While Loop -}
{- Notes:
 - A 'True' return value indicates a 'break' statement. -}
runWhile :: Expr -> [Statement] -> Evaluator Prim
runWhile x body = do
    ret <- runStatements body
    condition <- asCondition x
    let isBreak = ret /= MeowVoid 
    if condition && not isBreak
        then runWhile x body
        else return ret

{- Function Definition -}
runFuncDef :: Text.Text -> Args -> [Statement] -> Evaluator Prim
runFuncDef name args body = do
    let func = MeowFunc args body
    insertVar name func
    return MeowVoid
