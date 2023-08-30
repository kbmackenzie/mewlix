{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Interpreter.Interpret
( expression
, statement
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Data.Stack (Stack(..))
import Meowscript.Abstract.Meowable
import Meowscript.Abstract.Prettify
import Meowscript.Evaluate.Evaluator
import Meowscript.Evaluate.Exception
import Meowscript.Evaluate.Environment
import qualified Data.Text as Text
import qualified Meowscript.Data.Stack as Stack
import Meowscript.Interpreter.Exceptions
import Meowscript.Interpreter.Boxes
import Meowscript.Interpreter.Primitive
import Meowscript.Interpreter.Operations
import Meowscript.Parser.AST
import Control.Monad (void)
import qualified Data.List as List

type Meower a = Evaluator MeowAtom a

{- Expressions -}
---------------------------------------------------------------
expression :: Expr -> Meower MeowAtom
expression (ExprPrim a) = return (liftToMeow a)
expression (ExprKey k)  = asRef (SimpleKey k) >>= readRef

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
    closure <- context >>= freezeLocal -- Freeze local call frame.
    let arity = Stack.length params
    let body = Stack.singleton (StmtExpr expr)
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

expression (ExprPaw expr) = do
    ref <- asKey expr >>= asRef
    newValue <- readRef ref >>= flip meowAdd (MeowInt 1)
    writeRef newValue ref
    return newValue

expression (ExprClaw expr) = do
    ref <- asKey expr >>= asRef
    newValue <- readRef ref >>= flip meowSub (MeowInt 1)
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
            BinopListPush       -> meowPush
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
            UnopListPop         -> meowPop
            UnopListPeek        -> meowPeek
            UnopLen             -> meowLength
            UnopNot             -> return . meowNot
    f a

expression (ExprCall func args) = do
    undefined


identifier :: Expr -> Meower Identifier
identifier (ExprKey key) = return key
identifier other         = expression other >>= asIdentifier


{- References -}
---------------------------------------------------------------
data CatKey =
      SimpleKey Identifier
    | RefKey (Ref MeowAtom) Identifier
    | SingletonRef (Ref MeowAtom)

asRef :: CatKey -> Meower (Ref MeowAtom)
asRef (SimpleKey key) = lookUpRef key >>= \case
    Nothing     -> throwException =<< unboundException key []
    (Just ref)  -> return ref
asRef (RefKey ref key) = readRef ref >>= boxPeek key
asRef (SingletonRef ref) = return ref

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
asKey other = do
    ref <- expression other >>= newRef
    return (SingletonRef ref)

keyAssign :: CatKey -> MeowAtom -> Meower ()
keyAssign (SimpleKey key)    rvalue = context >>= contextWrite key rvalue
keyAssign (RefKey ref key)   rvalue = boxWrite key rvalue ref
keyAssign (SingletonRef ref) _      = throwException =<< notAnIdentifier . List.singleton =<< readRef ref


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
    ctx <- context :: Meower (Context MeowAtom)
    localContext ctx >>= runLocal m

statement :: Stack Statement -> Meower ReturnValue

statement Bottom = return ReturnVoid

statement ( (StmtExpr expr) ::| rest ) = do
    void (expression expr)
    statement rest

statement ( (StmtDeclaration key expr) ::| rest ) = do
    value <- expression expr
    context >>= contextWrite key value
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
    closure <- context :: Meower (Context MeowAtom)
    key <- asKey keyExpr
    name <- case key of
        (SimpleKey x)       -> return x
        (RefKey _  x)       -> return x
        (SingletonRef x)    -> throwException =<< notAFunctionName . List.singleton =<< readRef x
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
    let catcher :: CatException -> Meower ReturnValue
        catcher cat = do
            let num = (fromEnum . exceptionType) cat
            let expr = case maybeExpr of
                    Nothing  -> ExprPrim PrimNil
                    (Just x) -> x
            matched <- expression expr >>= \case
                (MeowInt a) -> return (fromIntegral a == num)
                _           -> return False
            if matched
                then statement catchBlock
                else throwException cat
    ret <- localBlock $ statement tryBlock `catchException` catcher
    case ret of
        ReturnVoid  -> statement rest
        other       -> return other

statement ( (StmtImport path maybeName) ::| rest ) = do
    statement rest
