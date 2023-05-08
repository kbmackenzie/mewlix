{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.Blocks
( evaluate
, isFuncDef
, isImport
) where

import Meowscript.Core.AST
import Meowscript.Core.Operations
import Meowscript.Core.Environment
import Meowscript.Core.Keys
import Meowscript.Core.Pretty
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
evaluate (ExpPrim prim) = return prim
evaluate (ExpBinop op a b) = join (binop op <$> evaluate a <*> evaluate b)
evaluate (ExpUnop op a) = evaluate a >>= unop op
evaluate (ExpList list) = mapM evaluate list <&> MeowList
evaluate (ExpObject object) = do
    let toPrim (key, expr) = (evaluate expr >>= ensureValue) <&> (key,)
    pairs <- mapM toPrim object
    MeowObject <$> createObject pairs
evaluate (ExpLambda args expr) = return (MeowFunc args [StmReturn expr])    
evaluate x@(ExpTrail {}) = MeowKey . KeyTrail <$> asTrail x
evaluate (ExpCall args funcKey) = evaluate funcKey >>= \case
    (MeowKey key) -> funcLookup key (funWrapper key args)
    lambda@(MeowFunc _ _) -> funWrapper (KeyNew "<lambda>") args lambda
    x -> throwError (notFunc x)
evaluate (ExpYarn expr) = evaluate expr >>= \case
    (MeowString str) -> (return . MeowKey . KeyNew) str
    x -> throwError (opException "Yarn" [x])

------------------------------------------------------------------------

{- Trails -}
asTrail :: Expr -> Evaluator [Text.Text]
asTrail x@(ExpTrail _ _) = unwrap x >>= mapM evaluate >>= mapM showMeow
asTrail _ = throwError "aa"

unwrap :: Expr -> Evaluator [Expr]
unwrap (ExpTrail x y) = (x:) <$> unwrap y
unwrap x = return [x]

{-- Helpers --}

-- Helper functions to distinguish between statements.
isFuncDef :: Statement -> Bool
{-# INLINABLE isFuncDef #-}
isFuncDef (StmFuncDef {}) = True
isFuncDef _ = False

isImport :: Statement -> Bool
{-# INLINABLE isImport #-}
isImport (StmImport {}) = True
isImport _ = False


------------------------------------------------------------------------

{-- Blocks --}

-- Run block in proper order: Function definitions, then other statements.
runBlock :: [Statement] -> Evaluator ReturnValue
{-# INLINABLE runBlock #-}
runBlock xs = do
    let (funcDefs, rest) = List.partition isFuncDef xs
    (void . runStatements) funcDefs
    runStatements rest

-- Running Statements
runStatements :: [Statement] -> Evaluator ReturnValue
runStatements [] = return RetVoid
runStatements ((StmReturn value):_) = RetValue <$> (evaluate value >>= ensureValue)
runStatements (StmBreak:_) = return RetBreak
runStatements (StmContinue:_) = return RetVoid
runStatements ((StmExpr expression):xs) = runExprStatement expression >> runStatements xs
runStatements ((StmFuncDef name args body):xs) = runFuncDef (KeyNew name) args body >> runStatements xs
runStatements (statement:xs) = runLocal $ do
    ret <- runTable statement
    if ret /= RetVoid
        then return ret
        else runStatements xs

-- Action table for all other statement blocks.
runTable :: Statement -> Evaluator ReturnValue
{-# INLINABLE runTable #-}
runTable (StmWhile a b) = runWhile a b
runTable (StmFor a b) = runFor a b
runTable (StmIfElse a b c) = runIfElse a b c
runTable (StmIf a b) = runIf a b
runTable (StmImport a _) = throwError (nestedImport a)
runTable _ = throwError "Invalid statement"

asCondition :: Expr -> Evaluator Bool
{-# INLINABLE asCondition #-}
asCondition x = meowBool <$> (evaluate x >>= ensureValue)

runExprStatement :: Expr -> Evaluator Prim
{-# INLINABLE runExprStatement #-}
runExprStatement = evaluate 

------------------------------------------------------------------------

{-- Functions --}
runFuncDef :: KeyType -> Params -> [Statement] -> Evaluator ReturnValue
runFuncDef key params body = do
    let func = MeowFunc params body
    assignment key func
    return RetVoid

addFunArgs :: [(Key, Expr)] -> Evaluator ()
{-# INLINABLE addFunArgs #-}
addFunArgs [] = return ()
addFunArgs ((key, expr):xs) = do
    evaluate expr >>= ensureValue >>= assignNew key
    addFunArgs xs

paramGuard :: KeyType -> Args -> Params -> Evaluator ()
{-# INLINABLE paramGuard #-}
paramGuard key args params = do
    when (length params < length args) ((throwError . manyArgs . showT) key)
    when (length params > length args) ((throwError . manyArgs . showT) key)

funWrapper :: KeyType -> Args -> FnCallback
funWrapper key args (MeowIFunc params fn) = do
    paramGuard key args params
    addFunArgs (zip params args)
    fn
funWrapper key args (MeowFunc params body) = do
    paramGuard key args params
    addFunArgs (zip params args)
    returnAsPrim <$> runBlock body
funWrapper _ _ _ = throwError "not function"

------------------------------------------------------------------------

{- If Else -}
runIf :: Expr -> [Statement] -> Evaluator ReturnValue
runIf x body = do
    condition <- asCondition x 
    if condition
      then runBlock body
      else return RetVoid

runIfElse :: Expr -> [Statement] -> [Statement] -> Evaluator ReturnValue
runIfElse x ifB elseB = do
    condition <- asCondition x
    if condition
        then runBlock ifB
        else runBlock elseB

{- While Loop -}
-- Notes: Any return value that isn't MeowVoid implies the end of the loop.
runWhile :: Expr -> [Statement] -> Evaluator ReturnValue
runWhile x body = do
    condition <- asCondition x
    if condition
        then innerWhile x body
        else return RetVoid

innerWhile :: Expr -> [Statement] -> Evaluator ReturnValue
innerWhile x body = runLocal $ do
    ret <- runBlock body
    condition <- asCondition x
    let shouldBreak = ret /= RetVoid
    if condition && not shouldBreak
        then innerWhile x body
        else return ret

{- For Loop -}
-- Notes: Any return value that isn't MeowVoid implies the end of the loop.
runFor :: (Expr, Expr, Expr) -> [Statement] -> Evaluator ReturnValue
runFor xs@(init', _, cond) body = do
    (void . evaluate) init'
    condition <- asCondition cond
    if condition
        then innerFor xs body
        else return RetVoid

innerFor :: (Expr, Expr, Expr) -> [Statement] -> Evaluator ReturnValue
innerFor xs@(_, incr, cond) body = runLocal $ do
    ret <- runBlock body
    (void . evaluate) incr
    condition <- asCondition cond
    let shouldBreak = ret /= RetVoid
    if condition && not shouldBreak
        then innerFor xs body
        else return ret
