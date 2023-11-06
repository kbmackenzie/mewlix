{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Interpreter.Interpret
( expression
, statement
, ReturnValue(..)
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Data.Key (Key)
import Mewlix.Data.Stack (Stack(..))
import Mewlix.Abstract.Meowable
import Mewlix.Abstract.Prettify
import Mewlix.Abstract.State
import Mewlix.Parser.Keywords (meowHome)
import qualified Data.Text as Text
import qualified Mewlix.Data.Stack as Stack
import Mewlix.Interpreter.Exceptions
import Mewlix.Interpreter.Boxes
import Mewlix.Interpreter.Primitive
import Mewlix.Interpreter.Operations
import Mewlix.Parser.AST
import Control.Monad (void)
import Control.Monad.Except (MonadError)
import qualified Data.HashMap.Strict as HashMap
import Mewlix.IO.Print (printTextLn)

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
    toMeow items

expression (ExprBox pairs) = do
    items <- mapM (mapM expression) pairs
    (toMeow . MeowPairs . Stack.toList) items

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
    (Singleton a)   -> meowAdd a (MeowInt 1)
    key             -> do
        a        <- keyLookup key
        newValue <- meowAdd a (MeowInt 1)
        keyAssign key newValue
        return newValue

expression (ExprClaw expr) = asKey expr >>= \case
    (Singleton a)   -> meowSub a (MeowInt 1)
    key             -> do
        a        <- keyLookup key
        newValue <- meowSub a (MeowInt 1)
        keyAssign key newValue
        return newValue

expression (ExprPush left right) = asKey right >>= \case
    (Singleton b)   -> expression left >>= flip meowPush b
    key             -> do
        b   <- keyLookup key
        a   <- expression left
        newValue <- meowPush a b
        keyAssign key newValue
        return newValue

expression (ExprPop expr) = asKey expr >>= \case
    (Singleton a)   -> meowPop a
    key             -> do
        a        <- keyLookup key
        newValue <- meowPop a
        keyAssign key newValue
        return newValue

-- Boxes:
expression expr@(ExprDotOp _ _) = asKey expr >>= keyLookup
expression expr@(ExprBoxAccess _ _) = asKey expr >>= keyLookup

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

expression (ExprCall args argCount expr) = do
    key <- asKey expr
    case key of
        (SimpleKey name)  -> lookUp name >>= \case
                (MeowFunc f)  -> callFunction name argCount args f
                (MeowIFunc f) -> callInnerFunc name argCount args f
                other         -> throwError =<< notAFuncionException [other]
        (BoxKey box name) -> keyLookup key >>= \case
                (MeowFunc f)  -> callMethod name argCount args f box
                (MeowIFunc f) -> callInnerFunc name argCount args f
                other         -> throwError =<< notAFuncionException [other]

        -- Singleton calls:
        (Singleton a)     -> case a of
                (MeowFunc f)  -> callFunction (funcName f) argCount args f
                (MeowIFunc f) -> callInnerFunc (ifuncName f) argCount args f
                other         -> throwError =<< notAFuncionException [other]

identifier :: Expr -> Evaluator Key
identifier (ExprKey key) = return key
identifier other         = do
    value <- expression other
    throwError =<< notAnIdentifier [value]


{- References -}
---------------------------------------------------------------
data CatKey =
      SimpleKey     Key
    | BoxKey        CatBox Key
    | Singleton     MeowPrim

keyAsBox :: CatKey -> Evaluator CatBox
keyAsBox (SimpleKey key)    = lookUp key >>= asBox
keyAsBox (Singleton x)      = asBox x
keyAsBox (BoxKey box key)   = boxPeek key box >>= readRef >>= asBox

asKey :: Expr -> Evaluator CatKey
asKey (ExprKey key) = return (SimpleKey key)
asKey (ExprDotOp a b) = do
    box <- asKey a >>= keyAsBox
    key <- identifier b
    return (BoxKey box key)
asKey (ExprBoxAccess a b) = do
    box <- asKey a >>= keyAsBox
    key <- expression b >>= showMeow
    return (BoxKey box key)
asKey other = Singleton <$> expression other

keyLookup :: CatKey -> Evaluator MeowPrim
keyLookup (SimpleKey key)  = lookUp key
keyLookup (Singleton a)    = return a
keyLookup (BoxKey box key) = boxPeek key box >>= readRef

keyAssign :: CatKey -> MeowPrim -> Evaluator ()
keyAssign (SimpleKey key)    rvalue = contextWrite key rvalue
keyAssign (BoxKey box key)   rvalue = boxWrite key rvalue box
keyAssign (Singleton a)      _      = throwError =<< notAnIdentifier [a]


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
    | ReturnCont
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
                    ReturnCont  -> loop
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
                    ReturnCont  -> expression incr >> loop
                    ReturnBreak -> return ReturnVoid
                    other       -> return other
            else return ReturnVoid
    ret <- runLocal $ do
        void (liftedExpression decl)
        loop
    case ret of
        ReturnVoid -> statement rest
        other      -> return other

statement ( (StmtFuncDef (ParserFunc name params block) ) ::| rest ) = do
    closure <- asks evaluatorEnv
    let function = MeowFunc $ MeowFunction
            { funcArity   = Stack.length params
            , funcName    = name
            , funcParams  = params
            , funcBody    = block
            , funcClosure = closure             }
    contextDefine name function
    statement rest

statement ( (StmtReturn expr) ::| _ ) = do
    value <- expression expr
    return (ReturnPrim value)

statement ( StmtBreak    ::| _ ) = return ReturnBreak
statement ( StmtContinue ::| _ ) = return ReturnCont

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

statement _ = undefined -- todo!

{- Functions -}
---------------------------------------------------------------
liftReturn :: (MonadError CatException m) => ReturnValue -> m MeowPrim
liftReturn (ReturnPrim a) = return a
liftReturn ReturnVoid     = return MeowNil
liftReturn ReturnBreak    = throwError =<< undefined --todo: unexpected break
liftReturn ReturnCont     = throwError =<< undefined --todo: unexpected continue

bindArgs :: Params -> Stack MeowPrim -> Evaluator ()
bindArgs params prims = do
    let zipped = zip (Stack.toList params) (Stack.toList prims)
    let assign (key, value) = do
            contextDefine key value
    mapM_ assign zipped

paramGuard :: Key -> Int -> Int -> Evaluator ()
paramGuard key a b = case a `compare` b of
    EQ -> return ()
    LT -> throwError (arityException "Not enough arguments" key)
    GT -> throwError (arityException "Too many arguments" key)

callFunction :: Key -> Int -> Stack Expr -> MeowFunction -> Evaluator MeowPrim
callFunction key arity exprs function = stackTrace key $ do
    paramGuard key arity (funcArity function)
    let closure = funcClosure function
    values <- mapM expression exprs
    runClosure closure $ do
        bindArgs (funcParams function) values
        statement (funcBody function) >>= liftReturn

callMethod :: Key -> Int -> Stack Expr -> MeowFunction -> CatBox -> Evaluator MeowPrim
callMethod key arity exprs function parent = stackTrace key $ do
    paramGuard key arity (funcArity function)
    let closure = funcClosure function
    values <- mapM expression exprs
    runClosure closure $ do
        bindArgs (funcParams function) values
        contextDefine meowHome $ MeowBox parent
        statement (funcBody function) >>= liftReturn

callInnerFunc :: Key -> Int -> Stack Expr -> MeowIFunction -> Evaluator MeowPrim
callInnerFunc key arity exprs f = stackTrace key $ do
    paramGuard key arity (ifuncArity f)
    values <- mapM expression exprs
    runLocal $ do
        bindArgs (ifuncParams f) values
        ifunc f

stackTrace :: Key -> Evaluator a -> Evaluator a
stackTrace key m = m `catchError` addStackTrace key

addStackTrace :: Key -> CatException -> Evaluator a
addStackTrace key exc = do
    let message = Text.append (exceptionMessage exc) $ Text.concat
            [ "\n    In function \"", key, "\"" ]
    throwError exc { exceptionMessage = message }
