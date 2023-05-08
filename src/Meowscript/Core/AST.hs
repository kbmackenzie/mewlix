{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.AST
( Prim(..)
, Expr(..)
, Unop(..)
, Binop(..)
, Statement(..)
, ReturnValue(..)
, PrimRef
, ObjectMap
, Name
, Key
, Params
, Environment
, Evaluator
, InnerFunc
, Condition
, Block
, Qualified
) where

import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Data.IORef

type PrimRef = IORef Prim
type ObjectMap = Map.Map Key PrimRef

type Name = Text.Text
type Key = Text.Text
type Params = [Key]
type InnerFunc = Evaluator Prim

type Environment = IORef ObjectMap
type Evaluator a = ReaderT Environment (ExceptT Text.Text IO) a

data Prim =
      MeowString Text.Text
    | MeowRef PrimRef
    | MeowBool Bool
    | MeowInt Int
    | MeowDouble Double
    | MeowLonely
    | MeowList [PrimRef]
    | MeowFunc Params [Statement]
    | MeowObject ObjectMap
    | MeowIFunc Params InnerFunc
    | MeowModule ObjectMap

{- This is extremely different from actually pretty-printing Meowscript values!!!
 - Since Meowscript handles IORefs as primitives, a pretty-printing function
 - wrapped in an IO monad has to be used, and this 'show' instance is far from enough. ><
 - Thus, I'm using this to facilitate my understanding instead. -}
instance Show Prim where
    show (MeowString x) = Text.unpack x
    show (MeowRef _) = "<reference>"
    show (MeowInt x) = show x
    show (MeowBool x) = show x
    show (MeowDouble x) = show x
    show MeowLonely = "<lonely>"
    show (MeowList _) = "<list>"
    show (MeowFunc _ _) = "<func>"
    show (MeowObject _) = "<object>"
    show (MeowIFunc _ _) = "<inner-func>"
    show (MeowModule _) = "<module>"

data Expr =
      ExpPrim Prim
    | ExpKey Key
    | ExpUnop Unop Expr 
    | ExpBinop Binop Expr Expr
    | ExpList [Expr]
    | ExpObject [(Key, Expr)]
    | ExpLambda [Key] Expr
    | ExpCall [Expr] Expr
    | ExpTrail Expr Expr
    deriving (Show)

type Condition = Expr
type Block = [Statement]
type Qualified = Maybe Text.Text

data Statement =
      StmExpr Expr
    | StmWhile Condition Block
    | StmFor (Expr, Expr, Expr) Block
    | StmIf Condition Block
    | StmIfElse Condition Block Block
    | StmFuncDef Name Params [Statement]
    | StmReturn Expr
    | StmImport FilePath (Maybe Key)
    | StmIFunc Name Prim
    | StmContinue
    | StmBreak
    deriving (Show)

data Unop =
      MeowYarn 
    | MeowLen 
    | MeowKnockOver
    | MeowPeek
    | MeowNot 
    | MeowNegate
    | MeowPaw
    | MeowClaw
    deriving (Show)

data Binop =
      MeowAdd
    | MeowSub 
    | MeowMul 
    | MeowDiv 
    | MeowMod
    | MeowAnd 
    | MeowOr 
    | MeowCompare [Ordering]
    | MeowAssign 
    | MeowConcat 
    | MeowPush
    deriving (Show)

data ReturnValue =
      RVoid
    | RBreak
    | RValue Prim
    deriving (Show)

instance Eq ReturnValue where
    RVoid == RVoid = True
    RBreak == RBreak = True
    RValue _ == RValue _ = True
    _ == _ = False
