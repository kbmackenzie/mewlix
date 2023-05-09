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
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Reader (ask, asks, liftIO)
import Control.Monad.Except (throwError)
import Control.Monad (void, join, when)
import Data.Functor ((<&>))
import Data.IORef (readIORef)

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
    MeowObject <$> (liftIO . createObject) pairs
evaluate (ExpLambda args expr) = asks (MeowFunc args [StmReturn expr])
evaluate x@(ExpTrail {}) = MeowKey . KeyTrail <$> asTrail x
evaluate (ExpCall args funcKey) = evaluate funcKey >>= \case
    (MeowKey key) -> funcLookup key args
    lambda@(MeowFunc {}) -> runFunc "<lambda>" args lambda
    x -> throwError (notFunc x)
evaluate (ExpYarn expr) = evaluate expr >>= \case
    (MeowString str) -> (return . MeowKey . KeyNew) str
    x -> throwError (opException "Yarn" [x])

------------------------------------------------------------------------

{- Trails -}
asTrail :: Expr -> Evaluator [Text.Text]
asTrail x@(ExpTrail _ _) = unwrap x >>= mapM evaluate >>= mapM asKey
asTrail _ = throwError "aa"

asKey :: Prim -> Evaluator Text.Text
asKey (MeowKey key) = case key of
    (KeyModify x) -> return x
    (KeyNew x) -> return x
    _ -> throwError "trail!!"
asKey _ = throwError "invalid key in trail!!"

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
runStatements (statement:xs) = do
    ret <- runLocal $ runTable statement
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
    | length keys <= 1 = throwError "Invalid trail"
    | otherwise = trailAction (init keys) $ \ref -> do
        let parent = ref
        let key = last keys
        fn <- evalRef ref >>= peekAsObject key >>= evalRef
        runMethod key args parent fn

meowFunc :: Key -> Params -> Args -> [Statement] -> Evaluator Prim
meowFunc key params args body = do
    paramGuard key args params
    addFunArgs (zip params args)
    returnAsPrim <$> runBlock body

iFunc :: Key -> Params -> Args -> InnerFunc -> Evaluator Prim
iFunc key params args fn = do
    paramGuard key args params
    addFunArgs (zip params args)
    fn
    
runFunc :: Key -> Args -> Prim -> Evaluator Prim
runFunc key args (MeowIFunc params fn) = runLocal $ iFunc key params args fn
runFunc key args (MeowFunc params body closure) = do 
    closure' <- (liftIO . readIORef) closure
    runClosure closure' $ meowFunc key params args body
runFunc _ _ _ = throwError "not function"

runMethod :: Key -> Args -> PrimRef -> Prim -> Evaluator Prim
runMethod key args _ (MeowIFunc params fn) =
    runLocal $ iFunc key params args fn
runMethod key args parent (MeowFunc params body closure) = do 
    closure' <- (liftIO . readIORef) closure
    runClosure closure' $ do
        insertRef "home" parent
        meowFunc key params args body
runMethod _ _ _ _ = throwError "not function"

addFunArgs :: [(Key, Expr)] -> Evaluator ()
{-# INLINABLE addFunArgs #-}
addFunArgs [] = return ()
addFunArgs ((key, expr):xs) = do
    evaluate expr >>= ensureValue >>= assignNew key
    addFunArgs xs

paramGuard :: Key -> Args -> Params -> Evaluator ()
{-# INLINABLE paramGuard #-}
paramGuard key args params = do
    when (length params < length args) ((throwError . manyArgs) key)
    when (length params > length args) ((throwError . manyArgs) key)

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
