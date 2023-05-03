{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.Blocks
( evaluate
, runBlock
) where

import Meowscript.Core.AST
import Meowscript.Core.Operations
import Meowscript.Core.Environment
import Meowscript.Core.Exceptions
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Except (throwError)
import Control.Monad (void, join, when)
import Data.Functor ((<&>))

type Args = [Expr]

{- Evaluating Expressions -}
evaluate :: Expr -> Evaluator Prim
evaluate (EPrim prim) = return prim
evaluate (EBinop op a b) = join (binop op <$> evaluate a <*> evaluate b)
evaluate (EUnop op a) = evaluate a >>= unop op
evaluate (EList list) = mapM evaluate list <&> MeowList
evaluate (EObject object) = do
    let toPrim (key, expr) = (evaluate expr >>= ensureValue) <&> (key,)
    pairs <- mapM toPrim object
    (return . MeowObject) (Map.fromList pairs)
evaluate dots@(EDot {}) = unwrapDot dots <&> MeowTrail
evaluate (ELambda args expr) = return (MeowFunc args [SReturn expr])
    
{- Methods Call -}
evaluate (ECall args dots@(EDot {})) = do
    xs <- unwrapDot dots
    fn <- lookUpTrail xs
    let home = init xs -- All but the function name.
    let name = last xs
    runMethod name args home fn

{- Function Call -}
evaluate (ECall args name) = evaluate name >>= \case
    (MeowKey x) -> funCall x args
    -- Allow lambda immediate call:
    lambda@(MeowFunc _ _) -> runFunc "<lambda>" args lambda
    x -> throwError (notFunc x)


------------------------------------------------------------------------

{-- Helpers --}

-- Ensures a value will be passed instead of an atom.
ensureValue :: Prim -> Evaluator Prim
{-# INLINABLE ensureValue #-}
ensureValue (MeowKey x) = lookUpVar x >>= ensureValue
ensureValue (MeowTrail xs) = lookUpTrail xs >>= ensureValue
ensureValue x = return x

-- Converts the dot operator to a MeowTrail.
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

-- Helper functions to distinguish between statements.
isFuncDef :: Statement -> Bool
{-# INLINABLE isFuncDef #-}
isFuncDef (SFuncDef {}) = True
isFuncDef _ = False

isImport :: Statement -> Bool
{-# INLINABLE isImport #-}
isImport (SImport {}) = True
isImport _ = False

------------------------------------------------------------------------

{-- Blocks --}

-- Run block in proper order: Function definitions, then other statements.
runBlock :: [Statement] -> Evaluator Prim
{-# INLINABLE runBlock #-}
runBlock xs = do
    let (funcDefs, rest) = List.partition isFuncDef xs
    (void . runStatements) funcDefs
    runStatements rest

-- Running Statements
runStatements :: [Statement] -> Evaluator Prim
runStatements [] = return MeowVoid
runStatements ((SReturn value):_) = evaluate value >>= ensureValue
runStatements (SBreak:_) = return MeowBreak
runStatements (SContinue:_) = return MeowVoid
runStatements ((SExpr expression):xs) = runExprStatement expression >> runStatements xs
runStatements ((SFuncDef name args body):xs) = runFuncDef name args body >> runStatements xs
runStatements (statement:xs) = do
    stackPush
    ret <- runTable statement
    stackPop
    if ret /= MeowVoid
        then return ret
        else runStatements xs

-- Action table for all other statement blocks.
runTable :: Statement -> Evaluator Prim
{-# INLINABLE runTable #-}
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


------------------------------------------------------------------------

{-- Functions --}
runFuncDef :: Name -> Params -> [Statement] -> Evaluator Prim
runFuncDef name params body = do
    let func = MeowFunc params body
    insertVar name func
    return MeowVoid

addFunArgs :: [(Key, Expr)] -> Evaluator ()
{-# INLINABLE addFunArgs #-}
addFunArgs [] = return ()
addFunArgs ((key, expr):xs) = do
    evaluate expr >>= ensureValue >>= addToTop key
    addFunArgs xs

funCall :: Name -> Args -> Evaluator Prim
{-# INLINABLE funCall #-}
funCall name args = do
    x <- keyExists name
    if not x then throwError (badKey name)
    else lookUpVar name >>= runFunc name args

paramGuard :: Name -> Args -> Params -> Evaluator ()
{-# INLINABLE paramGuard #-}
paramGuard name args params = do
    when (length params < length args) (throwError $ manyArgs name)
    when (length params > length args) (throwError $ fewArgs name)

funWrapper :: Name -> Params -> Args -> Evaluator Prim -> Evaluator Prim
{-# INLINABLE funWrapper #-}
funWrapper name params args action = do
    paramGuard name args params
    stackPush
    addFunArgs (zip params args)
    ret <- action
    stackPop
    return ret

{- Function -}
runFunc :: Name -> Args -> Prim -> Evaluator Prim
runFunc name args (MeowFunc params body) = funWrapper name params args (runStatements body)
runFunc name args (MeowIFunc params fn) = funWrapper name params args fn
runFunc name _ _ = throwError (badFunc name)

{- Method -}
runMethod :: Name -> Args -> [Key] -> Prim -> Evaluator Prim
runMethod name args trail (MeowFunc params body) = funWrapper name params args $ do
    addToTop "home" (MeowTrail trail)
    runStatements body
runMethod name args trail (MeowIFunc params fn) = funWrapper name params args $ do
    addToTop "home" (MeowTrail trail)
    fn
runMethod name _ _ _ = throwError (badFunc name)

------------------------------------------------------------------------

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
