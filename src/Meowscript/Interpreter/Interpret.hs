{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Meowscript.Interpreter.Interpret
( expression
, statement
, ReturnValue(..)
) where

import Meowscript.Abstract.Meow
import Meowscript.Data.Ref
import Meowscript.Data.Stack (Stack(..))
import Meowscript.Abstract.Meowable
import Meowscript.Abstract.Prettify
import Meowscript.Abstract.State
import qualified Data.Text as Text
import qualified Meowscript.Data.Stack as Stack
import Meowscript.Interpreter.Exceptions
import Meowscript.Interpreter.Boxes
import Meowscript.Interpreter.Primitive
import Meowscript.Interpreter.Operations
import Meowscript.Parser.AST
import Control.Monad (void)
import Control.Monad.Except (MonadError)
import qualified Data.HashMap.Strict as HashMap
import Meowscript.IO.Print (printTextLn)

{- Expressions -}
---------------------------------------------------------------
expression :: Expr -> Evaluator MeowPrim
expression (ExprPrim a) = return (liftToMeow a)
expression (ExprKey k)  = lookUp k

-- Boolean operations:
expression (ExprAnd a b) = do
    value <- expression a
    if meowBool value
        then expression b
        else return value

expression (ExprOr a b) = do
    value <- expression a
    if meowBool value
        then return value
        else expression b

-- Ternary operation:
expression (ExprTernary condition a b) = do
    value <- expression condition
    if meowBool value
        then expression a
        else expression b

-- Atom generators:
expression (ExprList exprs) = do
    items <- mapM expression exprs
    (return . MeowStack . listToBoxedStack) items

expression (ExprBox pairs) = do
    let eval (key, expr) = (key,) <$> expression expr
    items <- mapM eval pairs
    (toMeow . MeowPairs) items

expression (ExprLambda params expr) = do
    closure <- asks evaluatorEnv -- >>= freezeLocal -- Freeze local call frame.
    let arity = Stack.length params
    let body = Stack.singleton (StmtReturn expr)
    let function = MeowFunction {
        funcArity = arity,
        funcParams = params,
        funcName = "<lambda>",
        funcBody = body,
        funcClosure = closure
    }
    return (MeowFunc function)

-- Assignment:
expression (ExprAssign left right) = do
    lref   <- asKey left
    rvalue <- expression right
    keyAssign lref rvalue
    return rvalue

expression (ExprPaw expr) = asKey expr >>= \case
    (SingletonAtom a) -> meowAdd a (MeowInt 1)
    key               -> do
        ref      <- asRef key
        a        <- readRef ref
        newValue <- meowAdd a (MeowInt 1)
        writeRef newValue ref
        return newValue

expression (ExprClaw expr) = asKey expr >>= \case
    (SingletonAtom a) -> meowSub a (MeowInt 1)
    key               -> do
        ref      <- asRef key
        a        <- readRef ref
        newValue <- meowSub a (MeowInt 1)
        writeRef newValue ref
        return newValue

expression (ExprPush exprA exprB) = asKey exprA >>= \case
    (SingletonAtom a) -> expression exprB >>= meowPush a
    key               -> do
        ref <- asRef key
        a   <- readRef ref
        b   <- expression exprB
        newValue <- meowPush a b
        writeRef newValue ref
        return newValue

expression (ExprPop expr) = asKey expr >>= \case
    (SingletonAtom a) -> meowPop a
    key               -> do
        ref      <- asRef key
        a        <- readRef ref
        newValue <- meowPop a
        writeRef newValue ref
        return newValue

-- Boxes:
expression (ExprDotOp boxExpr expr) = do
    box <- asKey boxExpr >>= asRef >>= readRef
    key <- identifier expr
    boxPeek key box >>= readRef

expression (ExprBoxAccess boxExpr expr) = do
    box <- asKey boxExpr >>= asRef >>= readRef
    key <- expression expr >>= showMeow
    boxPeek key box >>= readRef

-- Binary/unary operations:
expression (ExprBinop op exprA exprB) = do
    a <- expression exprA
    b <- expression exprB
    let f = case op of
            BinopAdd            -> meowAdd
            BinopSub            -> meowSub
            BinopMul            -> meowMul
            BinopDiv            -> meowDiv
            BinopMod            -> meowMod
            BinopPow            -> meowPow
            BinopConcat         -> meowConcat
            BinopCompareEq      -> meowEq
            BinopCompareLess    -> meowLesser
            BinopCompareGreat   -> meowGreater
            BinopCompareNotEq   -> meowNotEq
            BinopCompareLEQ     -> meowLEQ
            BinopCompareGEQ     -> meowGEQ
    f a b

expression (ExprUnop op exprA) = do
    a <- expression exprA
    let f = case op of
            UnopNegate          -> meowNegate
            UnopListPeek        -> meowPeek
            UnopLen             -> meowLength
            UnopNot             -> return . meowNot
    f a

expression (ExprCall expr args argCount) = do
    key <- asKey expr
    case key of
        -- Simple keys:
        (SimpleKey name)  -> lookUp name >>= \case
                (MeowFunc f)  -> callFunction name argCount args f
                (MeowIFunc f) -> callInnerFunc name argCount args f
                x             -> throwError =<< notAFuncionException [x]

        -- Box lookup:
        (RefKey ref name) -> asRef key >>= readRef >>= \case
                (MeowFunc f)  -> callMethod name argCount args f ref
                (MeowIFunc f) -> callInnerFunc name argCount args f
                x             -> throwError =<< notAFuncionException [x]

        -- Singleton calls:
        (SingletonAtom a) -> case a of
                (MeowFunc f)  -> callFunction (funcName f) argCount args f
                (MeowIFunc f) -> callInnerFunc (ifuncName f) argCount args f
                x             -> throwError =<< notAFuncionException [x]

identifier :: Expr -> Evaluator Identifier
identifier (ExprKey key) = return key
identifier other         = expression other >>= asIdentifier


{- References -}
---------------------------------------------------------------
data CatKey =
      SimpleKey Identifier
    | RefKey (Ref MeowPrim) Identifier
    | SingletonAtom MeowPrim

asRef :: CatKey -> Evaluator (Ref MeowPrim)
asRef (SimpleKey key) = lookUpRef key >>= \case
    Nothing     -> throwError (unboundException key)
    (Just ref)  -> return ref
asRef (RefKey ref key) = readRef ref >>= boxPeek key
asRef (SingletonAtom a) = newRef a

asKey :: Expr -> Evaluator CatKey
asKey (ExprKey key) = return (SimpleKey key)
asKey (ExprDotOp box expr) = do
    ref <- asKey box >>= asRef
    key <- identifier expr
    return (RefKey ref key)
asKey (ExprBoxAccess box expr) = do
    ref <- asKey box >>= asRef
    key <- expression expr >>= showMeow
    return (RefKey ref key)
asKey other = SingletonAtom <$> expression other

keyAssign :: CatKey -> MeowPrim -> Evaluator ()
keyAssign (SimpleKey key)    rvalue = contextWrite key rvalue
keyAssign (RefKey ref key)   rvalue = boxWrite key rvalue ref
keyAssign (SingletonAtom a)  _      = throwError =<< notAnIdentifier [a]


{- Lifted Expressions -}
---------------------------------------------------------------
liftedExpression :: LiftedExpr -> Evaluator MeowPrim
liftedExpression (LiftExpr expr) = expression expr
liftedExpression (LiftDecl key expr) = do
    value <- expression expr
    contextDefine key value
    return value


{- Statements -}
---------------------------------------------------------------
data ReturnValue =
      ReturnPrim MeowPrim
    | ReturnVoid
    | ReturnBreak

statement :: Stack Statement -> Evaluator ReturnValue
statement Bottom = return ReturnVoid

statement ( (StmtExpr expr) ::| rest ) = do
    void (expression expr)
    statement rest

statement ( (StmtDeclaration key expr) ::| rest ) = do
    value <- expression expr
    contextDefine key value
    statement rest

statement ( (StmtIfElse condExpr ifBlock elseBlock) ::| rest ) = do
    condition <- expression condExpr
    let block = if meowBool condition then ifBlock else elseBlock
    ret <- runLocal (statement block) 
    case ret of
        ReturnVoid  -> statement rest
        other       -> return other

statement ( (StmtWhile condExpr block) ::| rest ) = do
    let loop :: Evaluator ReturnValue
        loop = do
            condition <- expression condExpr
            if meowBool condition then do 
                ret <- runLocal (statement block)
                case ret of
                    ReturnVoid  -> loop
                    ReturnBreak -> return ReturnVoid
                    other       -> return other
            else return ReturnVoid
    ret <- loop
    case ret of
        ReturnVoid -> statement rest
        other      -> return other

statement ( (StmtFor (decl, incr, condExpr) block) ::| rest ) = do
    let loop :: Evaluator ReturnValue
        loop = do
            condition <- expression condExpr
            if meowBool condition then do
                ret <- runLocal (statement block)
                case ret of
                    ReturnVoid  -> expression incr >> loop
                    ReturnBreak -> return ReturnVoid
                    other       -> return other
            else return ReturnVoid
    ret <- runLocal $ do
        void (liftedExpression decl)
        loop
    case ret of
        ReturnVoid -> statement rest
        other      -> return other

statement ( (StmtFuncDef keyExpr params block) ::| rest ) = do
    closure <- asks evaluatorEnv
    key <- asKey keyExpr
    name <- case key of
        (SimpleKey x)       -> return x
        (RefKey _  x)       -> return x
        (SingletonAtom x)   -> throwError =<< notAFunctionName [x]
    let function = MeowFunc $ MeowFunction
            { funcArity   = Stack.length params
            , funcName    = name
            , funcParams  = params
            , funcBody    = block
            , funcClosure = closure             }
    case key of
        (SimpleKey k) -> contextDefine k function
        other         -> keyAssign other function
    statement rest

statement ( (StmtReturn expr) ::| _ ) = do
    value <- expression expr
    return (ReturnPrim value)

statement ( StmtBreak    ::| _ ) = return ReturnBreak
statement ( StmtContinue ::| _ ) = return ReturnVoid

statement ( (StmtTryCatch tryBlock (maybeExpr, catchBlock)) ::| rest ) = do
    -- 'Is this exception catchable?'
    let canCatch :: Int -> Bool
        canCatch num = num > 0 && num < fromEnum MeowBadImport

    let catcher :: CatException -> Evaluator ReturnValue
        catcher cat = do
            let num = (fromEnum . exceptionType) cat
            let expr = case maybeExpr of
                    Nothing  -> ExprPrim PrimNil
                    (Just x) -> x
            matched <- expression expr >>= \case
                (MeowInt a) -> return (a == num)
                _           -> return False
            if canCatch num && matched
                then statement catchBlock
                else throwError cat

    ret <- runLocal $ statement tryBlock `catchError` catcher
    case ret of
        ReturnVoid  -> statement rest
        other       -> return other

statement ( (StmtImport _ _) ::| _ ) = do
    throwError $ unexpectedException "Nested import should never be parsed."

{- Functions -}
---------------------------------------------------------------
liftReturn :: (MonadError CatException m) => ReturnValue -> m MeowPrim
liftReturn (ReturnPrim a) = return a
liftReturn ReturnVoid     = return MeowNil
liftReturn ReturnBreak    = throwError =<< undefined --todo: unexpected break

bindArgs :: Params -> Stack MeowPrim -> Evaluator ()
bindArgs params prims = do
    let zipped = zip (Stack.toList params) (Stack.toList prims)
    let assign (key, value) = do
            contextDefine key value
    mapM_ assign zipped

paramGuard :: Identifier -> Int -> Int -> Evaluator ()
paramGuard key a b = case a `compare` b of
    EQ -> return ()
    LT -> throwError (arityException "Not enough arguments" key)
    GT -> throwError (arityException "Too many arguments" key)

callFunction :: Identifier -> Int -> Stack Expr -> MeowFunction -> Evaluator MeowPrim
callFunction key arity exprs function = stackTrace key $ do
    paramGuard key arity (funcArity function)
    let closure = funcClosure function
    values <- mapM expression exprs
    runClosure closure $ do
        bindArgs (funcParams function) values
        statement (funcBody function) >>= liftReturn

callMethod :: Identifier -> Int -> Stack Expr -> MeowFunction -> Ref MeowPrim -> Evaluator MeowPrim
callMethod key arity exprs function ref = stackTrace key $ do
    paramGuard key arity (funcArity function)
    let closure = funcClosure function
    values <- mapM expression exprs
    runClosure closure $ do
        bindArgs (funcParams function) values
        contextPush "home" ref
        statement (funcBody function) >>= liftReturn

callInnerFunc :: Identifier -> Int -> Stack Expr -> MeowIFunction -> Evaluator MeowPrim
callInnerFunc key arity exprs f = stackTrace key $ do
    paramGuard key arity (ifuncArity f)
    values <- mapM expression exprs
    runLocal $ do
        bindArgs (ifuncParams f) values
        ifunc f

stackTrace :: Identifier -> Evaluator a -> Evaluator a
stackTrace key m = m `catchError` addStackTrace key

addStackTrace :: Identifier -> CatException -> Evaluator a
addStackTrace key exc = do
    let message = Text.append (exceptionMessage exc) $ Text.concat
            [ "\n    In function \"", key, "\"" ]
    throwError exc { exceptionMessage = message }
