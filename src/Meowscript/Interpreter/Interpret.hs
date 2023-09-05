{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Interpreter.Interpret
( expression
, statement
, ReturnValue(..)
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Data.Stack (Stack(..))
import Meowscript.Abstract.Meowable
import Meowscript.Abstract.Prettify
import Meowscript.Evaluate.Evaluator
import Meowscript.Evaluate.Exception
import Meowscript.Evaluate.Environment
import Meowscript.Evaluate.State
import qualified Data.Text as Text
import qualified Meowscript.Data.Stack as Stack
import Meowscript.Interpreter.Exceptions
import Meowscript.Interpreter.Boxes
import Meowscript.Interpreter.Primitive
import Meowscript.Interpreter.Operations
import Meowscript.Parser.AST
import Control.Monad (void)
import qualified Data.HashMap.Strict as HashMap
import Meowscript.IO.Print (printTextLn)

type Meower a = Evaluator MeowAtom a

{- Expressions -}
---------------------------------------------------------------
expression :: Expr -> Meower MeowAtom
expression (ExprPrim a) = return (liftToMeow a)
expression (ExprKey k)  = do
    --liftIO (printTextLn "Key lookup:")
    --printContext
    lookUp k

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
    (return . MeowStack . boxList) items

expression (ExprBox pairs) = do
    let eval (key, expr) = (key,) <$> expression expr
    items <- mapM eval pairs
    (toMeow . MeowPairs) items

expression (ExprLambda params expr) = do
    closure <- context -- >>= freezeLocal -- Freeze local call frame.
    --liftIO (printTextLn "Lambda def:")
    --printContext
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
    --liftIO (printTextLn "Call:")
    --printContext
    case key of
        -- Simple keys:
        (SimpleKey name)  -> lookUp name >>= \case
                (MeowFunc f)  -> callFunction name argCount args f
                (MeowIFunc f) -> callInnerFunc name argCount args f
                x             -> throwException =<< notAFuncionException [x]

        -- Box lookup:
        (RefKey ref name) -> asRef key >>= readRef >>= \case
                (MeowFunc f)  -> callMethod name argCount args f ref
                (MeowIFunc f) -> callInnerFunc name argCount args f
                x             -> throwException =<< notAFuncionException [x]

        -- Singleton calls:
        (SingletonAtom a) -> case a of
                (MeowFunc f)  -> callFunction (funcName f) argCount args f
                (MeowIFunc f) -> callInnerFunc (ifuncName f) argCount args f
                x             -> throwException =<< notAFuncionException [x]

identifier :: Expr -> Meower Identifier
identifier (ExprKey key) = return key
identifier other         = expression other >>= asIdentifier


{- References -}
---------------------------------------------------------------
data CatKey =
      SimpleKey Identifier
    | RefKey (Ref MeowAtom) Identifier
    | SingletonAtom MeowAtom

asRef :: CatKey -> Meower (Ref MeowAtom)
asRef (SimpleKey key) = lookUpRef key >>= \case
    Nothing     -> throwException (unboundException key)
    (Just ref)  -> return ref
asRef (RefKey ref key) = readRef ref >>= boxPeek key
asRef (SingletonAtom a) = newRef a

asKey :: Expr -> Meower CatKey
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

define :: Identifier -> MeowAtom -> Meower ()
define key rvalue = context >>= contextWrite key rvalue

keyAssign :: CatKey -> MeowAtom -> Meower ()
keyAssign (SimpleKey key)    rvalue = define key rvalue
keyAssign (RefKey ref key)   rvalue = boxWrite key rvalue ref
keyAssign (SingletonAtom a)  _      = throwException =<< notAnIdentifier [a]


{- Lifted Expressions -}
---------------------------------------------------------------
liftedExpression :: LiftedExpr -> Meower MeowAtom
liftedExpression (LiftExpr expr) = expression expr
liftedExpression (LiftDecl key expr) = do
    value <- expression expr
    context >>= contextWrite key value
    return value


{- Statements -}
---------------------------------------------------------------
data ReturnValue =
      ReturnAtom MeowAtom
    | ReturnVoid
    | ReturnBreak

localBlock :: Meower a -> Meower a
localBlock m = do
    ctx <- context
    localContext ctx >>= runLocal m

statement :: Stack Statement -> Meower ReturnValue

statement Bottom = return ReturnVoid

statement ( (StmtExpr expr) ::| rest ) = do
    void (expression expr)
    statement rest

statement ( (StmtDeclaration key expr) ::| rest ) = do
    value <- expression expr
    context >>= contextWrite key value
    --liftIO (printTextLn "Declaration:")
    --printContext
    statement rest

statement ( (StmtIfElse condExpr ifBlock elseBlock) ::| rest ) = do
    condition <- expression condExpr
    let block = if meowBool condition then ifBlock else elseBlock
    ret <- localBlock (statement block) 
    case ret of
        ReturnVoid  -> statement rest
        other       -> return other

statement ( (StmtWhile condExpr block) ::| rest ) = do
    let loop :: Meower ReturnValue
        loop = do
            condition <- expression condExpr
            if meowBool condition then do 
                ret <- localBlock (statement block)
                case ret of
                    ReturnVoid  -> loop
                    ReturnBreak -> return ReturnVoid
                    other       -> return other
            else return ReturnVoid
    ret <- loop
    case ret of
        ReturnVoid -> statement rest
        other      -> return other

statement ( (StmtFor (decl, condExpr, incr) block) ::| rest ) = do
    let loop :: Meower ReturnValue
        loop = do
            condition <- expression condExpr
            if meowBool condition then do
                ret <- localBlock (statement block)
                void (expression incr)
                case ret of
                    ReturnVoid  -> loop
                    ReturnBreak -> return ReturnVoid
                    other       -> return other
            else return ReturnVoid
    ret <- localBlock $ do
        void (liftedExpression decl)
        loop
    case ret of
        ReturnVoid -> statement rest
        other      -> return other

statement ( (StmtFuncDef keyExpr params block) ::| rest ) = do
    closure <- context
    key <- asKey keyExpr
    name <- case key of
        (SimpleKey x)       -> return x
        (RefKey _  x)       -> return x
        (SingletonAtom x)   -> throwException =<< notAFunctionName [x]
    let function = MeowFunc $ MeowFunction
            { funcArity   = Stack.length params
            , funcName    = name
            , funcParams  = params
            , funcBody    = block
            , funcClosure = closure             }
    keyAssign key function
    statement rest

statement ( (StmtReturn expr) ::| _ ) = do
    value <- expression expr
    return (ReturnAtom value)

statement ( StmtBreak    ::| _ ) = return ReturnBreak
statement ( StmtContinue ::| _ ) = return ReturnVoid

statement ( (StmtTryCatch tryBlock (maybeExpr, catchBlock)) ::| rest ) = do
    -- 'Is this exception catchable?'
    let canCatch :: Int -> Bool
        canCatch num = num > 0 && num < fromEnum MeowBadImport

    let catcher :: CatException -> Meower ReturnValue
        catcher cat = do
            let num = (fromEnum . exceptionType) cat
            let expr = case maybeExpr of
                    Nothing  -> ExprPrim PrimNil
                    (Just x) -> x
            matched <- expression expr >>= \case
                (MeowInt a) -> return (fromIntegral a == num)
                _           -> return False
            if canCatch num && matched
                then statement catchBlock
                else throwException cat

    ret <- localBlock $ statement tryBlock `catchException` catcher
    case ret of
        ReturnVoid  -> statement rest
        other       -> return other

statement ( (StmtImport _ _) ::| rest ) = do
    statement rest


{- Functions -}
---------------------------------------------------------------
liftReturn :: (MeowThrower m) => ReturnValue -> m MeowAtom
liftReturn (ReturnAtom a) = return a
liftReturn ReturnVoid     = return MeowNil
liftReturn ReturnBreak    = throwException =<< undefined --todo: unexpected break

bindArgs :: Params -> Stack Expr -> Meower ()
bindArgs params exprs = do
    let zipped = zip (Stack.toList params) (Stack.toList exprs)
    let assign (key, expr) = do
            value <- expression expr
            context >>= contextWrite key value
    mapM_ assign zipped
    --context >>= contextMany pairs

paramGuard :: Identifier -> Int -> Int -> Meower ()
paramGuard key a b = case a `compare` b of
    EQ -> return ()
    LT -> throwException (arityException "Too many arguments" key)
    GT -> throwException (arityException "Not enough arguments" key)

localClosure :: Context MeowAtom-> Meower a -> Meower a
localClosure closure m = localContext closure >>= runLocal m

callFunction :: Identifier -> Int -> Stack Expr -> MeowFunction -> Meower MeowAtom
callFunction key arity exprs function = stackTrace key $ do
    paramGuard key arity (funcArity function)
    let closure = funcClosure function
    --localClosure closure $ do
    localBlock $ do
        --liftIO (printTextLn "Closure:")
        --printContext
        bindArgs (funcParams function) exprs
        statement (funcBody function) >>= liftReturn

callMethod :: Identifier -> Int -> Stack Expr -> MeowFunction -> Ref MeowAtom -> Meower MeowAtom
callMethod key arity exprs function ref = stackTrace key $ do
    paramGuard key arity (funcArity function)
    let closure = funcClosure function
    localClosure closure $ do
        bindArgs (funcParams function) exprs
        context >>= contextDefine "home" ref
        statement (funcBody function) >>= liftReturn

callInnerFunc :: Identifier -> Int -> Stack Expr -> MeowIFunction -> Meower MeowAtom
callInnerFunc key arity exprs f = stackTrace key $ do
    paramGuard key arity (ifuncArity f)
    localBlock $ do
        bindArgs (ifuncParams f) exprs
        ifunc f

stackTrace :: Identifier -> Meower a -> Meower a
stackTrace key m = m `catchException` addStackTrace key

addStackTrace :: Identifier -> CatException -> Meower a
addStackTrace key exc = do
    let message = Text.append (exceptionMessage exc) $ Text.concat
            [ "\n    In function \"", key, "\"" ]
    throwException exc { exceptionMessage = message }



{- Debugging -}
--------------------------------------------------------------------
printContext :: Evaluator MeowAtom ()
printContext = do
    ctx <- askState evaluatorCtx
    let printCtx :: Context MeowAtom -> Evaluator MeowAtom ()
        printCtx c = do
            keys <- HashMap.keys . getEnv <$> readRef (currentEnv ctx)
            liftIO $ printTextLn $ Text.concat [ "Keys:", (Text.pack . show) keys ]
            case enclosingCtx c of
                Nothing -> liftIO (printTextLn "[Context End]")
                (Just c') -> printCtx c'
    printCtx ctx
