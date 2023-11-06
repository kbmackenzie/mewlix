{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Parser.AST 
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
-- Utils:
, isImport
, fromImport
) where

import Mewlix.Data.Stack (Stack)
import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.Utils.Types

type Identifier = Text

data ParserPrim =
      PrimInt Int
    | PrimStr Text
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
    | ExprList (Stack Expr)
    | ExprBox (Stack (Identifier, Expr))
    | ExprAssign Expr Expr
    | ExprPaw Expr
    | ExprClaw Expr
    | ExprPush Expr Expr
    | ExprPop Expr
    | ExprLambda Params Expr
    | ExprCall (Stack Expr) Int Expr
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
type Params = Stack Text
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
    | StmtFuncDef Identifier Params Block
    | StmtDeclaration Identifier Expr
    | StmtImport FilePathT (Maybe Identifier)
    | StmtReturn Expr
    | StmtBreak
    | StmtContinue
    | StmtTryCatch Block CatchBlock
    deriving (Show)


{- Utils -}
----------------------------------------------------------------------------------
isImport :: Statement -> Bool
{-# INLINE isImport #-}
isImport (StmtImport _ _) = True
isImport _                = False

fromImport :: Statement -> (FilePath, Maybe Identifier)
{-# INLINE fromImport #-}
fromImport (StmtImport path key) = (Text.unpack path, key)
fromImport _ = error "Mewlix.Parser.AST.fromImport: Statement isn't an import!"
-- This error should *never* happen!!! I'm adding it just for tidiness.
