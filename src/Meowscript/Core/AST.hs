{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.AST
( Prim(..)
, Expr(..)
, Unop(..)
, Binop(..)
, Statement(..)
, Args
, Name
, Key
, asString
, asBool
, asInt
, asDouble
) where

import qualified Data.Text as Text
import qualified Data.Map as Map

type Args = [Text.Text]
type Key = Text.Text
type Name = Text.Text

type ObjectMap = Map.Map Text.Text Prim

type InnerFunc = (Prim -> Prim -> Prim)
type InnerIO = (Prim -> Prim -> IO Prim)

data Prim =
      MeowString Text.Text
    | MeowKey Key
    | MeowBool Bool
    | MeowInt Int
    | MeowDouble Double
    | MeowLonely
    | MeowList [Prim]
    | MeowFunc Args [Statement]
    | MeowLambda Args Expr
    | MeowObject ObjectMap
    | MeowBreak
    | MeowVoid
    | MeowIFunc Args InnerFunc
    | MeowIO Args InnerIO

instance Eq Prim where
    {- Lonely (nil) -}
    MeowLonely == MeowLonely = True
    MeowLonely == _ = False
    _ == MeowLonely = False
    {- Basic Types -}
    (MeowString a) == b = a == asString b
    (MeowInt a) == b = a == asInt b
    (MeowDouble a) == b = a == asDouble b
    (MeowBool a) == b = a == asBool b
    {- Lists -}
    (MeowList a) == (MeowList b) = a == b
    (MeowList []) == b = MeowLonely == b
    (MeowList (a:_)) == b = a == b
    {- Objects -}
    (MeowObject a) == (MeowObject b) = a == b
    (MeowObject _) == _ = False
    {-------------------------------------}
    {- Anything else should never be compared.
    - I didn't't wanna make it 'undefined', though.
    - And I didn't want to make them all evaluate to False, 
    - as that would generate inconsistencies with Ord.
    - (Are they all simultaneously lesser/greater than each other?)
    -
    - I also didn't want to manually implement some weird comparison logic
    - to each of them, though. How do you compare two IO functions?
    -
    - So I'm giving them this absurd logic-breaking definition instead.
    - Read it as 'All inner types are equal to each other.'
    - A good way to check if a variable holds a function, if anything. -}
    _ == _ = True

instance Ord Prim where
    {- Lonely (nil) -}
    -- It's always less than anything else.
    compare MeowLonely MeowLonely = EQ
    compare MeowLonely _ = LT
    compare _ MeowLonely = GT
    {- Basic Types -}
    -- Allowing some type conversion.
    compare (MeowString a) b = compare a (asString b)
    compare (MeowInt a) b = compare a (asInt b) 
    compare (MeowDouble a) b = compare a (asDouble b)
    compare (MeowBool a) b = compare a (asBool b)
    {- Lists -}
    {- An empty list is equivalent to 'lonely'. -}
    compare (MeowList a) (MeowList b) = compare a b
    compare (MeowList []) b = compare MeowLonely b
    compare (MeowList (a:_)) b = compare a b
    {- Objects -}
    compare (MeowObject a) (MeowObject b) = compare a b
    {- All inner types count as being equal to each other.
     - Read my comment about it in Prim's Eq instance declaration. -}
    compare _ _ = EQ

instance Show Prim where
    {- Basic Types -}
    show (MeowString x) = Text.unpack x
    show (MeowKey x) = concat ["key <", Text.unpack x, ">"]
    show (MeowBool x) = if x then "yummy" else "icky"
    show (MeowInt x) = show x
    show (MeowDouble x) = show x
    show (MeowList x) = show x
    {- Lonely -}
    show MeowLonely = "lonely"
    {- Inner Types (should never be shown) -}
    show (MeowFunc {}) = "<function>"
    show (MeowLambda {}) = "<lambda-function>"
    show (MeowObject _) = "<map-object>"
    show MeowBreak = "<break>"
    show MeowVoid = "<void>"
    show (MeowIFunc {}) = "<inner-function>"
    show (MeowIO {}) = "<inner-function>"

data Expr =
      EPrim Prim
    | EUnop Unop Expr 
    | EBinop Binop Expr Expr
    | EList [Expr]
    | EObject [(Key, Expr)]
    | ECall [Expr] Expr
    | EDot Expr Expr
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
    | MeowNot 
    | MeowNegate
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


{- Stringify -}
asString :: Prim -> Text.Text

asString (MeowString a) = a
asString x = (Text.pack . show) x

{- Boolean-ify -}
asBool :: Prim -> Bool

asBool (MeowBool a) = a
asBool MeowLonely = False
asBool (MeowString "") = False
asBool (MeowList []) = False
asBool _ = True

{- Int-ify -}
asInt :: Prim -> Int
asInt (MeowInt a) = a
asInt (MeowDouble a) = floor a
asInt (MeowBool a) = if a then 1 else 0
asInt (MeowList a) = length a
asInt (MeowObject a) = Map.size a
asInt _ = 0

{- Double-ify -}
asDouble :: Prim -> Double
asDouble (MeowInt a) = fromIntegral a
asDouble (MeowDouble a) = a
asDouble (MeowBool a) = if a then 1 else 0
asDouble a@(MeowList _) = (fromIntegral . asInt) a
asDouble a@(MeowObject _) = (fromIntegral . asInt) a
asDouble _ = 0
