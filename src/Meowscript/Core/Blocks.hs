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
import Data.IORef (readIORef)

type Args = [Prim]

{- Evaluating Expressions -}
evaluate :: Expr -> Evaluator Prim
evaluate (ExpPrim prim) = return prim
evaluate (ExpBinop op a b) = join (binop op <$> evaluate a <*> evaluate b)
evaluate (ExpUnop op a) = evaluate a >>= unop op
evaluate (ExpList list) = mapM evaluate list <&> MeowList
evaluate (ExpObject object) = do
    let toPrim (key, expr) = (evaluate expr >>= ensureValue) <&> (key,)
    pairs <- mapM toPrim object
    MeowObject <$> (liftIO . createObject) pairs
evaluate (ExpLambda args expr) = asks (MeowFunc args [StmReturn expr])
evaluate x@(ExpTrail {}) = MeowKey . KeyTrail <$> asTrail x

-- Function call
evaluate (ExpCall args funcKey) = do
    args' <- mapM (evaluate >=> ensureValue) args
    evaluate funcKey >>= \case
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
    
------------------------------------------------------------------------

{- Trails -}
asTrail :: Expr -> Evaluator [Text.Text]
asTrail x@(ExpTrail _ _) = (unwrap x >>= mapM evaluate . reverse) >>= mapM asKey
asTrail x = throwError $ meowUnexpected "Token misinterpreted as trail!" (showT x)

asKey :: Prim -> Evaluator Text.Text
asKey (MeowKey key) = case key of
    (KeyModify x) -> return x
    (KeyNew x) -> return x
    (KeyTrail xs) -> throwError $ meowUnexpected "Nested trail!" (showT xs)
asKey x = showMeow x >>= throwError . badTrail

unwrap :: Expr -> Evaluator [Expr]
unwrap (ExpTrail x y) = (y:) <$> unwrap x
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

boolEval :: Expr -> Evaluator Bool
{-# INLINABLE boolEval #-}
boolEval x = (evaluate >=> ensureValue) x <&> meowBool

------------------------------------------------------------------------

funcTrace :: Key -> Args -> Evaluator a -> Evaluator a
funcTrace key args = stackTrace $ do
    args' <- mapM prettyMeow args <&> Text.intercalate ", "
    return $ Text.concat [ "In function '", key, "'. Arguments: [", args', "]" ]

------------------------------------------------------------------------

{-- Blocks --}

-- Run block in proper order: Function definitions, then other statements.
runBlock :: Block -> IsLoop -> Evaluator ReturnValue
{-# INLINABLE runBlock #-}
runBlock xs isLoop = do
    let (funcDefs, rest) = List.partition isFuncDef xs
    void $ runStatements funcDefs False
    runStatements rest isLoop

-- Running Statements
runStatements :: Block -> IsLoop -> Evaluator ReturnValue
runStatements [] _ = return RetVoid
runStatements ((StmReturn value):_) _ = RetValue <$> (evaluate value >>= ensureValue)
runStatements (StmBreak:_) isLoop = if isLoop
                                        then return RetBreak
                                        else throwError (notInLoop meowBreak)
runStatements (StmContinue:_) isLoop = if isLoop
                                        then return RetVoid
                                        else throwError (notInLoop meowContinue)
runStatements ((StmExpr expression):xs) isLoop =
    runExprStatement expression >> runStatements xs isLoop
runStatements ((StmFuncDef name args body):xs) isLoop =
    runFuncDef (KeyNew name) args body >> runStatements xs isLoop
runStatements (statement:xs) isLoop = do
    ret <- runLocal $ runTable statement
    if ret /= RetVoid
        then return ret
        else runStatements xs isLoop

-- Action table for all other statement blocks.
runTable :: Statement -> Evaluator ReturnValue
{-# INLINABLE runTable #-}
runTable (StmWhile a b) = runWhile a b
runTable (StmFor a b) = runFor a b
runTable (StmIfElse a b c) = runIfElse a b c
runTable (StmIf a b) = runIf a b
runTable (StmImport a _) = throwError (nestedImport a)
runTable x = throwError ("Critical failure: Invalid statement. Trace: " `Text.append` showT x)

runExprStatement :: Expr -> Evaluator Prim
{-# INLINABLE runExprStatement #-}
runExprStatement = evaluate 

------------------------------------------------------------------------

{-- Functions --}
runFuncDef :: KeyType -> Params -> Block -> Evaluator ReturnValue
runFuncDef key params body = do 
    asks (MeowFunc params body) >>= assignment key
    return RetVoid

{- Run Functions -}
funcLookup :: KeyType -> Args -> Evaluator Prim
funcLookup key args = case key of 
    (KeyModify x) -> runMeow x
    (KeyNew x) -> runMeow x
    (KeyTrail xs) -> asMethod xs args
    where runMeow x = lookUp x >>= runFunc x args

asMethod :: [Key] -> Args -> Evaluator Prim
asMethod keys args
    | length keys <= 1 = throwError (shortTrail keys)
    | otherwise = trailAction (init keys) $ \ref -> do
        let parent = ref
        let key = last keys
        fn <- evalRef ref >>= peekAsObject key >>= evalRef
        runMethod key args parent fn

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
    closure' <- (liftIO . readIORef) closure
    runClosure closure' $ meowFunc key params args body
runFunc key _ _ = throwError (badFunc key)

runMethod :: Key -> Args -> PrimRef -> Prim -> Evaluator Prim
runMethod key args _ (MeowIFunc params fn) =
    runLocal $ iFunc key params args fn
runMethod key args parent (MeowFunc params body closure) = funcTrace key args $ do 
    closure' <- (liftIO . readIORef) closure
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
runIf :: Condition -> Block -> Evaluator ReturnValue
runIf x body = do
    condition <- boolEval x 
    if condition
      then runBlock body False
      else return RetVoid

runIfElse :: Condition -> Block -> Block -> Evaluator ReturnValue
runIfElse x ifB elseB = do
    condition <- boolEval x
    if condition
        then runBlock ifB False
        else runBlock elseB False

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
    let shouldBreak = ret /= RetVoid
    if condition && not shouldBreak
        then innerWhile x body
        else return ret

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
    let shouldBreak = ret /= RetVoid
    if condition && not shouldBreak
        then innerFor xs body
        else return ret
