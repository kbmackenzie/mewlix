{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Interpreter.Interpret
(
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Data.Stack (Stack)
import Meowscript.Abstract.Meowable
import Meowscript.Abstract.Prettify
import Meowscript.Evaluate.Evaluator
import Meowscript.Evaluate.Environment
import qualified Data.Text as Text
import qualified Meowscript.Data.Stack as Stack
import Meowscript.Interpreter.Exceptions
import Meowscript.Interpreter.Boxes
import Meowscript.Interpreter.Primitive
import Meowscript.Parser.AST

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
    lref   <- asKey left >>= asRef
    rvalue <- expression right
    writeRef rvalue lref
    return rvalue

-- Boxes:
expression (ExprDotOp boxExpr expr) = do
    box <- asKey boxExpr >>= asRef >>= readRef
    key <- identifier expr
    boxPeek key box >>= readRef

expression (ExprBoxAccess boxExpr expr) = do
    box <- asKey boxExpr >>= asRef >>= readRef
    key <- expression expr >>= showMeow
    boxPeek key box >>= readRef
 
{-data Expr =
      ExprPrim ParserPrim
    | ExprKey Identifier
    | ExprAnd Expr Expr
    | ExprOr Expr Expr
    | ExprBinop Binop Expr Expr
    | ExprUnop Unop Expr
    | ExprTernary Expr Expr Expr
    | ExprList [Expr]
    | ExprBox [(Identifier, Expr)]
    | ExprAssign Expr Expr
    | ExprLambda Params Expr
    | ExprCall Expr (Stack Expr)
    | ExprDotOp Expr Expr
    | ExprBoxAccess Expr Expr
    deriving (Show)
 -}

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
