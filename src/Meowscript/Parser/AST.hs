{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Meowscript.Parser.AST 
( ParserPrim(..)
, Expr(..)
, Binop(..)
, Unop(..)
, LiftedExpr(..)
, Statement(..)
, Identifier
, Block
, Params
, CatchBlock
) where

import qualified Data.Text as Text
import Meowscript.Utils.Types
import Meowscript.Data.Stack (Stack)

type Identifier = Text.Text

data ParserPrim =
      PrimInt Int
    | PrimStr Text.Text
    | PrimFloat Double
    | PrimBool Bool
    | PrimNil
    deriving (Show)

data Expr =
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
    | ExprPaw Expr
    | ExprClaw Expr
    | ExprPush Expr Expr
    | ExprPop Expr
    | ExprLambda Params Expr
    | ExprCall Expr (Stack Expr) Int
    | ExprDotOp Expr Expr
    | ExprBoxAccess Expr Expr
    deriving (Show)

data Binop =
      BinopAdd
    | BinopSub
    | BinopMul
    | BinopDiv
    | BinopMod
    | BinopPow
    | BinopConcat
    | BinopCompareEq
    | BinopCompareLess
    | BinopCompareGreat
    | BinopCompareNotEq
    | BinopCompareGEQ
    | BinopCompareLEQ
    deriving (Show)

data Unop =
      UnopNegate
    | UnopNot
    | UnopListPeek
    | UnopLen
    deriving (Show)

type Block  = Stack Statement
type Params = Stack Text.Text
type CatchBlock = (Maybe Expr, Block)

-- A 'lifted' expression type that allows declarations.
data LiftedExpr =
      LiftExpr Expr
    | LiftDecl Identifier Expr
    deriving (Show)

data Statement =
      StmtExpr Expr
    | StmtWhile Expr Block
    | StmtFor (LiftedExpr, Expr, Expr) Block
    | StmtIfElse Expr Block Block
    | StmtFuncDef Expr Params Block
    | StmtDeclaration Identifier Expr
    | StmtImport FilePathT (Maybe Identifier)
    | StmtReturn Expr
    | StmtBreak
    | StmtContinue
    | StmtTryCatch Block CatchBlock
    deriving (Show)
