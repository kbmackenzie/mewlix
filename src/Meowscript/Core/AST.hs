{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.AST
( Prim(..)
, Expr(..)
, Unop(..)
, Binop(..)
, Statement(..)
, Name
, Key
, Params
, ObjectMap
, Environment
, EnvStack
, Evaluator
, InnerFunc
, Condition
, Block
, Qualified
, showT
, asString
, asBool
, prettyMap
) where

import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)

type Name = Text.Text
type Key = Text.Text
type Params = [Text.Text]

type ObjectMap = Map.Map Text.Text Prim

type Environment = Map.Map Text.Text Prim
type EnvStack = [Environment]
type Evaluator a = StateT EnvStack (ExceptT Text.Text IO) a

type InnerFunc = Evaluator Prim

type Condition = Expr
type Block = [Statement]
type Qualified = Maybe Text.Text

data Prim =
      MeowString Text.Text
    | MeowKey Key
    | MeowBool Bool
    | MeowInt Int
    | MeowDouble Double
    | MeowLonely
    | MeowList [Prim]
    | MeowFunc Params [Statement]
    | MeowObject ObjectMap
    | MeowBreak
    | MeowVoid
    | MeowTrail [Key]
    | MeowIFunc Params InnerFunc

instance Eq Prim where
    (MeowInt a) == (MeowDouble b) = fromIntegral a == b
    (MeowInt a) == (MeowInt b) = a == b

    (MeowDouble a) == (MeowDouble b) = a == b
    (MeowDouble a) == (MeowInt b) = a == fromIntegral b

    (MeowString a) == (MeowString b) = a == b
    (MeowString a) == b = a == asString b
    a == (MeowString b) = asString a == b

    (MeowBool a) == (MeowBool b) = a == b
    (MeowBool a) == b = a == asBool b
    a == (MeowBool b) = asBool a == b

    (MeowList a) == (MeowList b) = a == b
    (MeowList a) == b = a == [b]
    a == (MeowList b) = [a] == b
    (MeowObject a) == (MeowObject b) = a == b

    MeowLonely == MeowLonely = True
    _ == MeowLonely = False
    MeowLonely == _ = False

    MeowVoid == MeowVoid = True
    MeowVoid == _ = False
    _ == MeowVoid = False

    MeowBreak == MeowBreak = True 
    MeowBreak == _ = False
    _ == MeowBreak = False

    -- Stringify everything else because they shouldn't
    -- be compared, LOL.
    a == b = asString a == asString b

instance Ord Prim where
    (MeowInt a) `compare` (MeowDouble b) = fromIntegral a `compare` b
    (MeowInt a) `compare` (MeowInt b) = a `compare` b

    (MeowDouble a) `compare` (MeowDouble b) = a `compare` b
    (MeowDouble a) `compare` (MeowInt b) = a `compare` fromIntegral b

    (MeowString a) `compare` (MeowString b) = a `compare` b
    (MeowString a) `compare` b = a `compare` asString b
    a `compare` (MeowString b) = asString a `compare` b

    (MeowBool a) `compare` (MeowBool b) = a `compare` b
    (MeowBool a) `compare` b = a `compare` asBool b
    a `compare` (MeowBool b) = asBool a `compare` b

    (MeowList a) `compare` (MeowList b) = a `compare` b
    (MeowList a) `compare` b = a `compare` [b]
    a `compare` (MeowList b) = [a] `compare` b
    (MeowObject a) `compare` (MeowObject b) = a `compare` b

    -- Lonely > Break > Void
    MeowLonely `compare` MeowLonely = EQ
    MeowLonely `compare` MeowBreak = GT
    MeowLonely `compare` MeowVoid = GT

    MeowBreak `compare` MeowBreak = EQ
    MeowBreak `compare` MeowVoid = GT
    MeowBreak `compare` MeowLonely = LT

    MeowVoid `compare` MeowVoid = EQ
    MeowVoid `compare` MeowBreak = LT
    MeowVoid `compare` MeowLonely = LT

    -- Lonely, Break, Void are smaller than all.
    _ `compare` MeowLonely = LT
    _ `compare` MeowBreak = LT
    _ `compare` MeowVoid = LT

    -- Stringify everything else, because they shouldn't
    -- be compared. c':
    a `compare` b = asString a `compare` asString b

    
instance Show Prim where
    show (MeowKey x) = concat ["key <", Text.unpack x, ">"]
    show (MeowString x) = Text.unpack x
    show (MeowBool x) = if x then "yummy" else "icky"
    show (MeowInt x) = show x
    show (MeowDouble x) = show x
    show (MeowList x) = Text.unpack (prettyList x)
    show (MeowObject x) = Text.unpack (prettyMap x)
    show MeowLonely = "lonely"
    -- Inner Types (should never be shown)
    show (MeowFunc {}) = "<function>"
    show MeowBreak = "<break>"
    show MeowVoid = "<void>"
    show (MeowTrail {}) = "<key-trail>"
    show (MeowIFunc {}) = "<inner-function>"

data Expr =
      EPrim Prim
    | EUnop Unop Expr 
    | EBinop Binop Expr Expr
    | EList [Expr]
    | EObject [(Key, Expr)]
    | ELambda [Key] Expr
    | ECall [Expr] Expr
    | EDot Expr Expr
    -- | ERead
    -- | EWrite Expr
    deriving (Eq, Show, Ord)

data Statement =
      SExpr Expr
    | SWhile Expr [Statement]
    | SFor (Expr, Expr, Expr) [Statement]
    | SIf Expr [Statement]
    | SIfElse Expr [Statement] [Statement] 
    | SFuncDef Name Params [Statement]
    | SReturn Expr
    | SImport FilePath (Maybe Key)
    | SIFunc Name Prim
    | SContinue
    | SBreak
    | SAll [Statement]
    deriving (Eq, Show, Ord)

data Unop =
      MeowYarn 
    | MeowLen 
    | MeowKnockOver
    | MeowPeek
    | MeowNot 
    | MeowNegate
    | MeowPaw
    | MeowClaw
    deriving (Eq, Show, Ord)

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
    deriving (Eq, Show, Ord)


{- Helpers -}

-- Pretty-print Maps
prettyMap :: ObjectMap -> Text.Text
prettyMap o = Text.concat ["[ ", pairs, " ]"]
    where
        lst = Map.toList o
        pair (key, val) = Text.concat [ key, ": ", asString' val ]
        pairs = Text.intercalate ", " (map pair lst)

prettyList :: [Prim] -> Text.Text
prettyList xs = Text.concat ["[ ", items, " ]"]
    where items = Text.intercalate ", " (map asString' xs)

prettyText :: Text.Text -> Text.Text
prettyText x = Text.concat ["\"", x, "\""]

-- Show as Text
showT :: (Show a) => a -> Text.Text
showT = Text.pack . show

-- Stringify
asString :: Prim -> Text.Text
{-# INLINABLE asString #-}
asString (MeowString a) = a
asString (MeowList a) = prettyList a
asString (MeowObject a) = prettyMap a
asString x = showT x

asString' :: Prim -> Text.Text
asString' (MeowString a) = prettyText a
asString' x = asString x

-- Boolean-ify
asBool :: Prim -> Bool
{-# INLINABLE asBool #-}
asBool (MeowBool a) = a
asBool (MeowList x) = (not . null) x
asBool (MeowString x) = (not . Text.null) x
asBool (MeowObject x) = (not . Map.null) x
asBool MeowLonely = False
asBool _ = True
