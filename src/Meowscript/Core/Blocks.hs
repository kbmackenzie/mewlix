{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.Blocks
( evaluate
, runBlock
, isFuncDef
, isImport
) where

import Meowscript.Core.AST
import Meowscript.Core.Operations
import Meowscript.Core.Environment
import Meowscript.Core.Keys
import Meowscript.Core.Pretty
import Meowscript.Core.Exceptions
import Meowscript.Parser.Keywords
import qualified Data.Text as Text
import qualified Data.List as List
import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Except (throwError)
import Control.Monad (void, join, when, (>=>))
import Data.Functor ((<&>))

type Args = [Prim]

{- Evaluating Expressions -}
evaluate :: Expr -> Evaluator Prim
evaluate (ExpPrim prim) = return prim
evaluate (ExpBinop op a b) = join (binop op <$> evaluate a <*> evaluate b)
evaluate (ExpUnop op a) = evaluate a >>= unop op
evaluate (ExpList list) = mapM (evaluate >=> ensureValue) list <&> MeowList
evaluate (ExpObject object) = do
    let toPrim (key, expr) = (evaluate expr >>= ensureValue) <&> (key,)
    pairs <- mapM toPrim object
    MeowObject <$> (liftIO . createObject) pairs
evaluate (ExpLambda args expr) = asks (MeowFunc args [StmReturn expr] . snd)

{-
-- Trail
evaluate x@(ExpTrail {}) = do
    (ref, expr) <- trailReduce x
    case expr of
        (ExpCall args funcKey) -> do
            args' <- mapM (evaluate >=> ensureValue) args
            fn <- evaluate funcKey
            funcLookup (KeyRef (ref, fn)) args'
        _ -> MeowKey . KeyRef . (ref,) <$> evaluate expr
-}

evaluate (ExpDotOp expr dot) = evaluate expr >>= \case
    (MeowKey key) -> MeowKey . KeyRef <$> ((,) <$> keyAsRef key <*> evaluate dot)
    obj@(MeowObject _) -> MeowKey . KeyRef <$> ((,) <$> newMeowRef obj <*> evaluate dot)
    _ -> throwError $ meowUnexpected "todo" "todo"

-- Function call
evaluate (ExpCall exprKey args) = do
    args' <- mapM (evaluate >=> ensureValue) args
    evaluate exprKey >>= \case
        (MeowKey key) -> funcLookup key args'
        lambda@(MeowFunc {}) -> runFunc "<lambda>" args' lambda
        x -> throwError =<< notFunc x

-- Yarn operator
evaluate (ExpYarn expr) = (evaluate >=> ensureValue) expr >>= \case
    (MeowString str) -> (return . MeowKey . KeyNew) str
    x -> throwError =<< opException "Yarn" [x]

-- Boolean operators (&&, ||)
evaluate (ExpMeowAnd exprA exprB) = boolEval exprA >>= \case
    False -> (return . MeowBool) False
    True -> MeowBool <$> boolEval exprB
evaluate (ExpMeowOr exprA exprB) = boolEval exprA >>= \case
    True -> (return . MeowBool) True
    False -> MeowBool <$> boolEval exprB

-- Ternary operator
evaluate (ExpTernary cond exprA exprB) = boolEval cond >>= \case
    True -> evaluate exprA
    False -> evaluate exprB

-- Box operator []
evaluate (ExpBoxOp boxExpr expr) = evaluate boxExpr >>= \case
    (MeowKey key) -> MeowKey . KeyRef <$> ((,) <$> keyAsRef key <*> evaluate expr)
    obj@(MeowObject _) -> MeowKey . KeyRef <$> ((,) <$> newMeowRef obj <*> evaluate expr)
    _ -> throwError $ meowUnexpected "todo" "todo"

------------------------------------------------------------------------

{- Trails -}

{-
trailReduce :: Expr -> Evaluator (PrimRef, Expr)
{-# INLINABLE trailReduce #-}
trailReduce (ExpTrail x y) = (,y) <$> innerTrail x
trailReduce other = throwError =<< (evaluate >=> badTrail . List.singleton) other

innerTrail :: Expr -> Evaluator PrimRef
{-# INLINABLE innerTrail #-}
innerTrail (ExpTrail x y) = case y of
    (ExpCall args funcKey) -> do
        args' <- mapM (evaluate >=> ensureValue) args
        fn <- evaluate funcKey
        innerTrail x >>= (flip funcLookup args' . KeyRef . (,fn)) >>= newMeowRef
    (ExpBoxOp boxExpr expr) -> innerTrail (ExpTrail (ExpTrail x boxExpr) expr)
    _ -> do
        obj <- (innerTrail >=> readMeowRef) x
        key <- (evaluate >=> ensureKey) y
        peekAsObject key obj
    --(,) <$> (innerTrail >=> readMeowRef) x <*> (evaluate >=> ensureKey) y >>= peekAsObject
innerTrail prim = evaluate prim >>= \case
    (MeowKey key) -> (extractKey >=> lookUpRef) key
    obj@(MeowObject _) -> newMeowRef obj
    other -> throwError =<< badTrail[other]
-}


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

boolEval :: Expr -> Evaluator Bool
{-# INLINABLE boolEval #-}
boolEval x = (evaluate >=> ensureValue) x <&> meowBool

------------------------------------------------------------------------

{-- Stack Tracing --}

funcTrace :: Key -> Args -> Evaluator a -> Evaluator a
funcTrace key args = stackTrace $ do
    args' <- mapM prettyMeow args <&> Text.intercalate ", "
    return $ Text.concat [ "In function '", key, "'. Arguments: [", args', "]" ]

------------------------------------------------------------------------

{-- Blocks --}

runBlock :: Block -> IsLoop -> Evaluator ReturnValue
{-# INLINABLE runBlock #-}
runBlock = runStatements

{-
-- Run block in proper order: Function definitions, then other statements.
-- ... We're going to do this later though.
-- Functions are already 'hoisted' so long as you don't call a function in
-- the global scope, so this is fine. I don't have to do all of that below.
-- 
-- I'm gonna keep the order of function definitions mattering exclusively if
-- you try to call a function *in the global scope* before defining it.
--
runBlock xs isLoop = do
    let (funcDefs, rest) = List.partition isFuncDef xs
    void $ runStatements funcDefs False
    runStatements rest isLoop
-}

-- Running Statements
runStatements :: Block -> IsLoop -> Evaluator ReturnValue
runStatements [] _ = return RetVoid
runStatements ((StmReturn value):_) _ = RetValue <$> (evaluate value >>= ensureValue)
runStatements (StmBreak:_) isLoop = if isLoop
                                        then return RetBreak
                                        else throwError (notInLoop meowBreak)
runStatements (StmContinue:_) isLoop = if isLoop
                                        then return RetContinue
                                        else throwError (notInLoop meowContinue)
runStatements ((StmExpr expression):xs) isLoop =
    runExprStatement expression >> runStatements xs isLoop
runStatements ((StmFuncDef name args body):xs) isLoop = evaluate name >>= \case
    (MeowKey key) -> runFuncDef key args body >> runStatements xs isLoop
    x -> throwError =<< badFuncDef x
runStatements (statement:xs) isLoop = do
    ret <- runLocal $ runTable statement isLoop
    if ret /= RetVoid
        then return ret
        else runStatements xs isLoop

-- Action table for all other statement blocks.
runTable :: Statement -> IsLoop -> Evaluator ReturnValue
{-# INLINABLE runTable #-}
runTable (StmWhile a b) _ = runWhile a b
runTable (StmFor a b) _ = runFor a b
runTable (StmIfElse as b) isLoop = runIfElse as b isLoop
runTable (StmIf as) isLoop = runIf as isLoop
runTable (StmImport a _) _ = throwError (nestedImport a)
runTable x _ = throwError ("Critical failure: Invalid statement. Trace: " `Text.append` showT x)

runExprStatement :: Expr -> Evaluator Prim
{-# INLINABLE runExprStatement #-}
runExprStatement = evaluate 

------------------------------------------------------------------------

{-- Functions --}
runFuncDef :: KeyType -> Params -> Block -> Evaluator ReturnValue
runFuncDef key params body = do 
    asks (MeowFunc params body . snd) >>= assignment key
    return RetVoid

{- Run Functions -}
funcLookup :: KeyType -> Args -> Evaluator Prim
funcLookup key args = case key of 
    (KeyModify x) -> runMeow x
    (KeyNew x) -> runMeow x
    x@(KeyRef (ref, k)) -> do
        funcKey <- ensureKey k
        keyLookup x >>= runMethod funcKey args ref
    where runMeow x = lookUp x >>= runFunc x args

meowFunc :: Key -> Params -> Args -> [Statement] -> Evaluator Prim
meowFunc key params args body = do
    paramGuard key args params
    addFunArgs (zip params args)
    returnAsPrim <$> runBlock body False

iFunc :: Key -> Params -> Args -> InnerFunc -> Evaluator Prim
iFunc key params args fn = do
    paramGuard key args params
    addFunArgs (zip params args)
    fn
    
runFunc :: Key -> Args -> Prim -> Evaluator Prim
runFunc key args (MeowIFunc params fn) = runLocal $ iFunc key params args fn
runFunc key args (MeowFunc params body closure) = funcTrace key args $ do 
    closure' <- readMeowRef closure
    runClosure closure' $ meowFunc key params args body
runFunc key _ _ = throwError (badFunc key)

runMethod :: Key -> Args -> PrimRef -> Prim -> Evaluator Prim
runMethod key args _ (MeowIFunc params fn) =
    runLocal $ iFunc key params args fn
runMethod key args parent (MeowFunc params body closure) = funcTrace key args $ do 
    closure' <- readMeowRef closure
    runClosure closure' $ do
        insertRef "home" parent
        meowFunc key params args body
runMethod key _ _ _ = throwError (badFunc key)

addFunArgs :: [(Key, Prim)] -> Evaluator ()
{-# INLINABLE addFunArgs #-}
addFunArgs [] = return ()
addFunArgs ((key, prim):xs) = do
    assignNew key prim
    addFunArgs xs

paramGuard :: Key -> Args -> Params -> Evaluator ()
{-# INLINABLE paramGuard #-}
paramGuard key args params = do
    when (length params < length args) (throwError =<< manyArgs key args)
    when (length params > length args) (throwError =<< fewArgs  key args)

------------------------------------------------------------------------

{- If Else -}
runIf :: [MeowIf] -> IsLoop -> Evaluator ReturnValue
runIf [] _ = return RetVoid
runIf ((MeowIf cond body):xs) isLoop = do
    condition <- boolEval cond
    if condition
      then runBlock body isLoop
      else runIf xs isLoop

runIfElse :: [MeowIf] -> Block -> IsLoop -> Evaluator ReturnValue
runIfElse [] elseBlock isLoop = runBlock elseBlock isLoop
runIfElse ((MeowIf cond body):xs) elseBlock isLoop = do
    condition <- boolEval cond
    if condition
        then runBlock body isLoop
        else runIfElse xs elseBlock isLoop

{- While Loop -}
-- Notes: Any return value that isn't RetVoid implies the end of the loop.
runWhile :: Condition -> Block -> Evaluator ReturnValue
runWhile x body = do
    condition <- boolEval x
    if condition
        then innerWhile x body
        else return RetVoid

innerWhile :: Condition -> Block -> Evaluator ReturnValue
innerWhile x body = runLocal $ do
    ret <- runBlock body True
    condition <- boolEval x
    if condition && not (shouldBreak ret)
        then innerWhile x body
        else return $ case ret of
            RetContinue -> RetVoid
            RetBreak -> RetVoid
            ret' -> ret'

{- For Loop -}
-- Notes: Any return value that isn't RetVoid implies the end of the loop.
runFor :: (Expr, Expr, Expr) -> Block -> Evaluator ReturnValue
runFor xs@(init', _, cond) body = do
    (void . evaluate) init'
    condition <- boolEval cond
    if condition
        then innerFor xs body
        else return RetVoid

innerFor :: (Expr, Expr, Expr) -> Block -> Evaluator ReturnValue
innerFor xs@(_, incr, cond) body = runLocal $ do
    ret <- runBlock body True
    (void . evaluate) incr
    condition <- boolEval cond
    if condition && not (shouldBreak ret)
        then innerFor xs body
        else return $ case ret of
            RetContinue -> RetVoid
            RetBreak -> RetVoid
            ret' -> ret'
