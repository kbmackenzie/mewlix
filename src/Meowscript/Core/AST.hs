module Meowscript.Core.AST
( Prim(..)
, Expr(..)
, Unop(..)
, Binop(..)
, Statement(..)
, Args
, Name
, Key
) where

import qualified Data.Text as Text

type Args = [Text.Text]
type Key = Text.Text
type Name = Text.Text

data Prim =
      MeowString Text.Text
    | MeowKey Key
    | MeowBool Bool
    | MeowInt Int
    | MeowDouble Double
    | MeowLonely
    | MeowFunc Args [Statement]
    | MeowBreak
    | MeowVoid
    deriving (Eq, Ord)

instance Show Prim where
    show (MeowString x) = Text.unpack x
    show (MeowKey x) = Text.unpack x
    show (MeowBool x) = if x then "yummy" else "icky"
    show (MeowInt x) = show x
    show (MeowDouble x) = show x
    show MeowLonely = "lonely"
    show (MeowFunc _ _) = "<function>"
    show MeowBreak = "<break>"
    show MeowVoid = "<void>"

data Expr =
      EPrim Prim
    | EUnop Unop Expr 
    | EBinop Binop Expr Expr
    | ECall [Expr] Expr
    | ERead
    | EWrite Expr
    deriving (Eq, Show, Ord)

data Statement =
      SExpr Expr
    | SWhile Expr [Statement]
    | SOnlyIf Expr [Statement]
    | SIfElse Expr [Statement] [Statement] 
    | SFuncDef Name Args [Statement]
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
