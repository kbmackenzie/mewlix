module Meowscript.Core.AST
( Prim(..)
, Expr(..)
, Unop(..)
, Binop(..)
, Statement(..)
, Args
) where

import qualified Data.Text as Text

type Args = [Text.Text]

data Prim =
      MeowString Text.Text
    | MeowAtom Text.Text
    | MeowBool Bool
    | MeowInt Int
    | MeowDouble Double
    | MeowLonely
    | MeowFunc Args [Statement]
    | MeowBreak
    deriving (Eq, Ord)

instance Show Prim where
    show (MeowString x) = Text.unpack x
    show (MeowAtom x) = Text.unpack x
    show (MeowBool x) = if x then "yummy" else "icky"
    show (MeowInt x) = show x
    show (MeowDouble x) = show x
    show MeowLonely = "lonely"
    show x@(MeowFunc _ _) = show x -- "<function>"
    show x@MeowBreak = show x

data Expr =
      EPrim Prim
    | EUnop Unop Expr 
    | EBinop Binop Expr Expr
    | ECall [Expr] Expr
    | EWhitespace
    deriving (Eq, Show, Ord)

data Statement =
      SExpr Expr
    | SWhile Expr [Statement]
    | SOnlyIf Expr [Statement]
    | SIfElse Expr [Statement] [Statement] 
    | SFuncDef Text.Text Args [Statement]
    | SReturn Expr
    | SContinue
    | SBreak
    | SAll [Statement]
    deriving (Eq, Show, Ord)

data Unop =
      MeowYarn 
    | MeowLen 
    | MeowPoke 
    | MeowNudge
    | MeowNegate
    | MeowNot 
    | MeowPeek
    | MeowSneak
    deriving (Eq, Show, Ord)

data Binop =
      MeowAdd
    | MeowSub 
    | MeowMul 
    | MeowDiv 
    | MeowAnd 
    | MeowOr 
    | MeowCompare [Ordering]
    | MeowAssign 
    | MeowConcat 
    deriving (Eq, Show, Ord)
