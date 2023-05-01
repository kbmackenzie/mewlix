{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE LambdaCase #-}

module Meowscript.Core.Blocks
( runEvaluator 
, evaluate
, runStatements
, runBlock
) where

import Meowscript.Core.AST
import Meowscript.Core.Evaluate
import Meowscript.Core.Operations
import Meowscript.Core.Environment
import Meowscript.Core.Exceptions
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.State (liftIO, runStateT)
import Control.Monad.Except (throwError, runExceptT)
import Control.Monad (void, when)
import Data.Functor ((<&>))

{- Run all statements from root. -}
runEvaluator :: EnvStack -> Evaluator a -> IO (Either Text.Text (a, EnvStack))
runEvaluator env x = runExceptT (runStateT x env)

{- Evaluating Expressions -}
evaluate :: Expr -> Evaluator Prim
evaluate (EPrim prim) = return prim
evaluate (EBinop op expressionA expressionB) = do
    a <- evaluate expressionA
    b <- evaluate expressionB
    binop op a b
evaluate (EUnop op expA) = evaluate expA >>= unop op

evaluate (EList list) = mapM evaluate list <&> MeowList
evaluate (EObject object) = do
    let asPair key x = return (key, x)
    asPrims <- mapM (\(key, exp') -> evaluate exp' >>= ensureValue >>= asPair key) object
    let mapObject = Map.fromList asPrims
    (return . MeowObject) mapObject
evaluate x@(EDot {}) = unwrapDot x <&> MeowTrail

-- Lambda functions
evaluate (ELambda args expr) = return (MeowFunc args [SReturn expr])
    
-- Methods
evaluate (ECall args x@(EDot {})) = do
    xs <- unwrapDot x
    fn <- lookUpTrail xs
    let home = init xs -- All but the function name.
    let name = last xs
    runMethod args name home fn

-- Functions
evaluate (ECall args name) = evaluate name >>= \case
    (MeowKey x) -> funcCall x args
    _ -> throwError "Invalid function name!"

-- Inner Functions
evaluate ERead = liftIO TextIO.getLine <&> MeowString
evaluate (EWrite x) = do
    evaluate x >>= ensureValue >>= (liftIO . print)
    return MeowLonely


{-- Helpers --}

-- Ensures a value will be passed instead of an atom.
ensureValue :: Prim -> Evaluator Prim
{-# INLINABLE ensureValue #-}
ensureValue (MeowKey x) = lookUpVar x >>= ensureValue
ensureValue (MeowTrail xs) = lookUpTrail xs >>= ensureValue
ensureValue x = return x


{-- Objects --}
-- Dot Operator
unwrapDot :: Expr -> Evaluator [Text.Text]
unwrapDot (EDot x y) = do
    x' <- unwrapDot x
    y' <- unwrapDot y
    return (x' ++ y')
unwrapDot (EPrim (MeowTrail xs)) = return xs
unwrapDot (EPrim (MeowKey x)) = return [x]
unwrapDot x = evaluate x >>= \tok -> case tok of
    (MeowKey key) -> return [key]
    _ -> (throwError . badTrail . showT) tok


{-- Blocks --}

-- Run block in proper order: Function definitions, then other statements.
runBlock :: [Statement] -> Evaluator Prim
{-# INLINABLE runBlock #-}
runBlock xs = do
    let (funcDefs, rest) = List.partition isFuncDef xs
    void $ runStatements funcDefs
    runStatements rest

-- Running Statements
runStatements :: [Statement] -> Evaluator Prim
runStatements [] = return MeowVoid

runStatements ((SReturn value):_) = evaluate value >>= ensureValue
runStatements (SBreak:_) = return MeowBreak
runStatements (SContinue:_) = return MeowVoid

runStatements ((SExpr expression):xs) =
    runExprStatement expression >> runStatements xs

runStatements ((SFuncDef name args body):xs) =
    runFuncDef name args body >> runStatements xs

runStatements (statement:xs) = do
    stackPush
    ret <- runTable statement
    stackPop
    if ret /= MeowVoid
        then return ret
        else runStatements xs
    
runTable :: Statement -> Evaluator Prim
runTable (SWhile a b) = runWhile a b
runTable (SFor a b) = runFor a b
runTable (SIfElse a b c) = runIfElse a b c
runTable (SIf a b) = runIf a b
runTable (SImport a _) = throwError (badImport a)
runTable _ = return MeowVoid

asCondition :: Expr -> Evaluator Bool
{-# INLINABLE asCondition #-}
asCondition x = asBool <$> (evaluate x >>= ensureValue)

runExprStatement :: Expr -> Evaluator Prim
{-# INLINABLE runExprStatement #-}
runExprStatement = evaluate 


{- If Else -}
runIf :: Expr -> [Statement] -> Evaluator Prim
runIf x body = do
    condition <- asCondition x 
    if condition
      then runBlock body
      else return MeowVoid

runIfElse :: Expr -> [Statement] -> [Statement] -> Evaluator Prim
runIfElse x ifB elseB = do
    condition <- asCondition x
    if condition
        then runBlock ifB
        else runBlock elseB


{- While Loop -}
-- Notes: Any return value that isn't MeowVoid implies the end of the loop.
runWhile :: Expr -> [Statement] -> Evaluator Prim
runWhile x body = do
    condition <- asCondition x
    if condition
        then innerWhile x body
        else return MeowVoid

innerWhile :: Expr -> [Statement] -> Evaluator Prim
innerWhile x body = do
    stackPush
    ret <- runBlock body
    stackPop
    condition <- asCondition x
    let isBreak = ret /= MeowVoid 
    if condition && not isBreak
        then innerWhile x body
        else return ret


{- For Loop -}
-- Notes: Any return value that isn't MeowVoid implies the end of the loop.
runFor :: (Expr, Expr, Expr) -> [Statement] -> Evaluator Prim
runFor xs@(init', _, cond) body = do
    (void . evaluate) init'
    condition <- asCondition cond
    if condition
        then innerFor xs body
        else return MeowVoid

innerFor :: (Expr, Expr, Expr) -> [Statement] -> Evaluator Prim
innerFor xs@(_, incr, cond) body = do
    stackPush
    ret <- runBlock body
    stackPop
    (void . evaluate) incr
    condition <- asCondition cond
    let isBreak = ret /= MeowVoid
    if condition && not isBreak
        then innerFor xs body
        else return ret


{-- Functions --}

runFuncDef :: Name -> Args -> [Statement] -> Evaluator Prim
runFuncDef name args body = do
    let func = MeowFunc args body
    insertVar name func
    return MeowVoid

-- Add function arguments to the environment.
funcArgs :: [(Key, Expr)] -> Evaluator ()
{-# INLINABLE funcArgs #-}
funcArgs [] = return ()
funcArgs ((key, expr):xs) = do
    evaluate expr >>= ensureValue >>= addToTop key
    funcArgs xs

funcCall :: Name -> [Expr] -> Evaluator Prim
{-# INLINABLE funcCall #-}
funcCall name args = do
    x <- keyExists name
    if not x then throwError (badKey name)
    else lookUpVar name >>= runFunc args name
        
runFunc :: [Expr] -> Text.Text -> Prim -> Evaluator Prim
runFunc args name (MeowFunc params body) = do
    when (length params < length args) (throwError $ manyArgs name)
    when (length params > length args) (throwError $ fewArgs name)
    stackPush
    funcArgs (zip params args)
    ret <- runStatements body
    stackPop
    return ret
runFunc _ name _ = throwError (badFunc name)

runMethod :: [Expr] -> Text.Text -> [Key] -> Prim -> Evaluator Prim
runMethod args name trail (MeowFunc params body) = do
    when (length params < length args) (throwError $ manyArgs name)
    when (length params > length args) (throwError $ fewArgs name)
    stackPush
    funcArgs (zip params args)
    addToTop "home" (MeowTrail trail)
    ret <- runStatements body
    stackPop
    return ret
runMethod _ name _ _ = throwError (badFunc name)

-- Helper functions to distinguish between statements.
isFuncDef :: Statement -> Bool
isFuncDef (SFuncDef {}) = True
isFuncDef _ = False
