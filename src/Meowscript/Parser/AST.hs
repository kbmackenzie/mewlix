{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Meowscript.Parser.AST 
( ParserPrim(..)
, Expr(..)
, Binop(..)
, Unop(..)
, Statement(..)
, Identifier
, Block
, Params
, CatchBlock
, liftToMeow
) where

import qualified Data.Text as Text
import Meowscript.Utils.Types
import Meowscript.Abstract.Atom

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
    | ExprLambda Params Expr
    | ExprCall Expr [Expr]
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
    | BinopListPush
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
    | UnopPaw
    | UnopClaw
    | UnopListPop
    | UnopListPeek
    | UnopLen
    deriving (Show)

type Block  = [Statement]
type Params = [Text.Text]
type CatchBlock = (Maybe Expr, Block)

data Statement =
      StmtExpr Expr
    | StmtIf Expr Block Block
    | StmtFor (Expr, Expr, Expr) Block
    | StmtWhile Expr Block
    | StmtFuncDef Expr Params Block
    | StmtDeclaration Identifier [Expr] Block
    | StmtImport FilePathT (Maybe Identifier)
    | StmtReturn Expr
    | StmtBreak
    | StmtContinue
    | StmtTryCatch Block CatchBlock
    deriving (Show)

liftToMeow :: ParserPrim -> MeowAtom
liftToMeow (PrimInt n) = (MeowInt . fromIntegral) n
liftToMeow (PrimStr s) = (MeowString . boxString) s
liftToMeow (PrimFloat f) = MeowFloat f
liftToMeow (PrimBool b) = MeowBool b
liftToMeow PrimNil = MeowNil
