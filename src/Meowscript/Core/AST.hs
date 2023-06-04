{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.AST
( Prim(..)
, Expr(..)
, Unop(..)
, Binop(..)
, Statement(..)
, KeyType(..)
, ReturnValue(..)
, PrimRef
, ObjectMap
, Key
, Overwrite
, Name
, Params
, Environment
, Closure
, Evaluator
, InnerFunc
, Condition
, Block
, Qualified
, IsLoop
, meowBool
, returnAsPrim
) where

import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Data.IORef
import Data.List (intercalate)

type PrimRef = IORef Prim
type ObjectMap = Map.Map Key PrimRef

type Key = Text.Text
type Overwrite = Bool
type InnerFunc = Evaluator Prim

type Environment = IORef ObjectMap
type Closure = Environment
type Evaluator a = ReaderT Environment (ExceptT Text.Text IO) a

data Prim =
      MeowString Text.Text
    | MeowKey KeyType
    | MeowBool Bool
    | MeowInt Int
    | MeowDouble Double
    | MeowLonely
    | MeowList [Prim]
    | MeowFunc Params [Statement] Closure
    | MeowObject ObjectMap
    | MeowIFunc Params InnerFunc

data KeyType =
      KeyModify Key
    | KeyNew Key
    | KeyTrail [Key]
    deriving (Eq)

{- This is extremely different from actually pretty-printing Meowscript values!!!
 - Since Meowscript handles IORefs as primitives, a pretty-printing function
 - has to be wrapped in an IO monad to be used, and this 'show' instance is far from enough. ><
 - Thus, I'm using this to facilitate my debugging instead. -}
instance Show Prim where
    show (MeowString x) = Text.unpack x
    show (MeowKey x) = concat ["<key: \"", show x, "\">" ]
    show (MeowInt x) = show x
    show (MeowBool x) = show x
    show (MeowDouble x) = show x
    show MeowLonely = "<lonely>"
    show (MeowList xs) = concat [ "[", (intercalate ", " . map show) xs, "]" ]
    show (MeowFunc {}) = "<func>"
    show (MeowObject _) = "<object>"
    show (MeowIFunc _ _) = "<inner-func>"

instance Show KeyType where
    show (KeyModify x) = Text.unpack x
    show (KeyNew x) = Text.unpack x
    show (KeyTrail xs) = (Text.unpack . Text.intercalate ".") xs

data Expr =
      ExpPrim Prim
    | ExpUnop Unop Expr 
    | ExpBinop Binop Expr Expr
    | ExpList [Expr]
    | ExpObject [(Key, Expr)]
    | ExpLambda [Key] Expr
    | ExpMeowAnd Expr Expr
    | ExpMeowOr Expr Expr
    | ExpCall [Expr] Expr
    | ExpTrail Expr Expr
    | ExpYarn Expr
    | ExpTernary Expr Expr Expr
    deriving (Show)

type Name = Text.Text
type Params = [Key]

type Condition = Expr
type Block = [Statement]
type Qualified = Maybe Text.Text
type IsLoop = Bool

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
      MeowLen 
    | MeowPeek
    | MeowKnockOver
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
    | MeowCompare [Ordering]
    | MeowAssign
    | MeowConcat 
    | MeowPush
    | MeowPow
    deriving (Show)

data ReturnValue =
      RetVoid
    | RetBreak
    | RetValue Prim
    deriving (Show)

instance Eq ReturnValue where
    RetVoid == RetVoid = True
    RetBreak == RetBreak = True
    RetValue _ == RetValue _ = True
    _ == _ = False

returnAsPrim :: ReturnValue -> Prim
returnAsPrim (RetValue x) = x
returnAsPrim _ = MeowLonely

{- A special case will have to be made when comparing objects later on! c':
 - Thanks to the IORefs, of course.
 - As of right now, I'm comparing the keys. -}
instance Eq Prim where
    -- Numbers
    (MeowInt a) == (MeowInt b) = a == b
    (MeowInt a) == (MeowDouble b) = fromIntegral a == b
    (MeowDouble a) == (MeowInt b) = a == fromIntegral b
    (MeowDouble a) == (MeowDouble b) = a == b

    -- Nil
    MeowLonely == MeowLonely = True
    _ == MeowLonely = False
    MeowLonely == _ = False

    -- Booleans
    (MeowBool a) == (MeowBool b) = a == b
    (MeowBool a) == b = a == meowBool b
    a == (MeowBool b) = meowBool a == b

    -- Strings
    (MeowString a) == (MeowString b) = a == b
    (MeowString a) == b = a == (Text.pack . show) b
    a == (MeowString b) = (Text.pack . show) a == b

    -- Lists, objects
    (MeowList a) == (MeowList b) = a == b
    (MeowObject a) == (MeowObject b) = Map.keys a == Map.keys b

    -- Stringify everything else because they shouldn't be compared c':
    a == b = show a == show b

instance Ord Prim where
    -- Numbers
    (MeowInt a) `compare` (MeowInt b) = a `compare` b
    (MeowInt a) `compare` (MeowDouble b) = fromIntegral a `compare` b
    (MeowDouble a) `compare` (MeowInt b) = a `compare` fromIntegral b
    (MeowDouble a) `compare` (MeowDouble b) = a `compare` b

    -- Nil
    MeowLonely `compare` MeowLonely = EQ
    _ `compare` MeowLonely = GT
    MeowLonely `compare` _ = LT

    -- Booleans
    (MeowBool a) `compare` (MeowBool b) = a `compare` b
    (MeowBool a) `compare` b = a `compare` meowBool b
    a `compare` (MeowBool b) = meowBool a `compare` b

    -- Strings
    (MeowString a) `compare` (MeowString b) = a `compare` b
    (MeowString a) `compare` b = a `compare` (Text.pack . show) b
    a `compare` (MeowString b) = (Text.pack . show) a `compare` b

    -- Lists, objects
    (MeowList a) `compare` (MeowList b) = a `compare` b
    (MeowObject a) `compare` (MeowObject b) = Map.keys a `compare` Map.keys b

    -- Stringify everything else because they shouldn't be compared c':
    a `compare` b = show a `compare` show b

meowBool :: Prim ->  Bool
meowBool (MeowBool a) = a
meowBool (MeowList a) = (not . null) a
meowBool (MeowString a) = (not . Text.null) a
meowBool (MeowObject a) = (not . Map.null) a
meowBool MeowLonely = False
meowBool _ = True
