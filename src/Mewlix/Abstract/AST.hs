{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Abstract.AST 
( Primitive(..)
, Block(..)
, Params(..)
, Arguments(..)
, Expression(..)
, Statement(..)
, BinaryOp(..)
, UnaryOp(..)
, MewlixFunction(..)
, MewlixClass(..)
, LineNumber(..)
) where

import Mewlix.Abstract.Key (Key)
import Data.Text (Text)
import Mewlix.Abstract.Module (Module)

data Primitive =
      MewlixInt             Int
    | MewlixString          Text
    | MewlixFloat           Double
    | MewlixBool            Bool
    | MewlixNil
    | MewlixHome
    | MewlixSuper
    deriving (Show)

data Expression =
      PrimitiveExpr         Primitive
    | Identifier            Key
    | ObjectProperty        Key
    | BooleanAnd            Expression Expression
    | BooleanOr             Expression Expression
    | BinaryOperation       BinaryOp Expression Expression
    | UnaryOperation        UnaryOp Expression
    | TernaryOperation      Expression Expression Expression
    | ListExpression        [Expression]
    | BoxExpression         [(Key, Expression)]
    | Assignment            Expression Expression
    | PawType               Expression
    | ClawEntries           Expression
    | ListPush              Expression Expression
    | ListPop               Expression
    | LambdaExpression      Params Expression
    | FunctionCall          Expression Arguments
    | SuperCall             Arguments
    | ClowderCreate         Expression Arguments
    | DotExpression         Expression Expression
    | LookupExpression      Expression Expression
    | ThrowError            Expression
    deriving (Show)

data BinaryOp =
      Addition 
    | Subtraction
    | Multiplication
    | Division
    | Modulo
    | Power
    | ListConcat
    | Equal
    | LessThan
    | GreaterThan
    | NotEqual
    | GreaterOrEqual
    | LesserOrEqual
    deriving (Eq, Ord, Enum, Bounded, Show)

data UnaryOp =
      Negation
    | ListPeek
    | BooleanNot
    | LengthLookup
    deriving (Eq, Ord, Enum, Bounded, Show)

newtype Block     = Block     { getBlock      :: [Statement]  } deriving (Show, Semigroup, Monoid)
newtype Params    = Params    { getParams     :: [Key]        } deriving (Show, Semigroup, Monoid)
newtype Arguments = Arguments { getArguments  :: [Expression] } deriving (Show, Semigroup, Monoid)

data MewlixFunction = MewlixFunction
    { funcName      :: Key
    , funcParams    :: Params
    , funcBody      :: Block       }
    deriving (Show)

data MewlixClass = MewlixClass
    { className         :: Key
    , classExtends      :: Maybe Key
    , classConstructor  :: Maybe MewlixFunction
    , classMethods      :: [MewlixFunction]      }
    deriving (Show)

newtype LineNumber = LineNumber { getLineNumber :: Int }
    deriving (Eq, Ord, Show, Enum, Num)

data Statement =
      ExpressionStatement   Expression
    | WhileLoop             Expression Block
    | ForEachLoop           Expression Key Block
    | IfElse                Expression Block Block
    | FunctionDef           MewlixFunction
    | Binding               Key Expression
    | LocalBinding          Key Expression
    | ClassDef              MewlixClass
    | ImportStatement       Module
    | Return                Expression
    | TryCatch              Block (Maybe Key) Block
    | Break 
    | Continue
    deriving (Show)
