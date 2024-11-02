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
, MewlixEnum(..)
, Conditional(..)
, YarnBall(..)
) where

import Mewlix.Abstract.Key (Key)
import Data.Text (Text)
import Mewlix.Abstract.Module (ModuleKey, ModuleData)
import Data.List.NonEmpty (NonEmpty)
import Text.Megaparsec (SourcePos)

data Primitive =
      MewlixInt             Int
    | MewlixString          Text
    | MewlixFloat           Double
    | MewlixBool            Bool
    | MewlixNil
    | MewlixHome
    | MewlixOutside
    deriving (Eq, Show)

data Expression =
      PrimitiveExpr         Primitive
    | Identifier            Key
    | ObjectProperty        Key
    | BooleanAnd            Expression Expression
    | BooleanOr             Expression Expression
    | BinaryOperation       BinaryOp Expression Expression
    | UnaryOperation        UnaryOp Expression
    | TernaryOperation      Expression Expression Expression
    | ShelfExpression       [Expression]
    | BoxExpression         [(Key, Expression)]
    | AskType               Expression
    | IsInstance            Expression Expression
    | ClawEntries           Expression
    | LambdaExpression      Params Expression
    | FunctionCall          Expression Arguments
    | ClowderCreate         Expression Arguments
    | DotExpression         Expression Expression
    | LookupExpression      Expression Expression
    | MeowExpression        Expression
    deriving (Eq, Show)

data BinaryOp =
      Addition 
    | Subtraction
    | Multiplication
    | Division
    | FloorDivision
    | Modulo
    | Power
    | ShelfPush
    | StringConcat
    | Contains
    | Equal
    | LessThan
    | GreaterThan
    | NotEqual
    | GreaterOrEqual
    | LesserOrEqual
    deriving (Eq, Ord, Enum, Bounded, Show)

data UnaryOp =
      Minus
    | Plus
    | ShelfPop
    | ShelfPeek
    | BooleanNot
    | LengthLookup
    deriving (Eq, Ord, Enum, Bounded, Show)

newtype Block     = Block     { getBlock      :: [Statement]  } deriving (Eq, Show, Semigroup, Monoid)
newtype Params    = Params    { getParams     :: [Key]        } deriving (Eq, Show, Semigroup, Monoid)
newtype Arguments = Arguments { getArguments  :: [Expression] } deriving (Eq, Show, Semigroup, Monoid)

data MewlixFunction = MewlixFunction
    { funcName      :: Key
    , funcParams    :: Params
    , funcBody      :: Block       }
    deriving (Eq, Show)

data MewlixClass = MewlixClass
    { className         :: Key
    , classExtends      :: Maybe Key
    , classConstructor  :: Maybe MewlixFunction
    , classMethods      :: [MewlixFunction]      }
    deriving (Eq, Show)

data MewlixEnum = MewlixEnum
    { enumName :: Key
    , enumKeys :: [Key] }
    deriving (Eq, Show)

data Conditional = Conditional
    { conditionalExpression :: Expression
    , conditionalBlock      :: Block             }
    deriving (Eq, Show)

data Statement =
      ExpressionStatement   Expression
    | WhileLoop             Expression Block
    | ForEachLoop           Expression Key Block
    | IfElse                (NonEmpty Conditional) (Maybe Block)
    | FunctionDef           MewlixFunction
    | FunctionAssignment    Expression MewlixFunction
    | Variable              Key Expression
    | Constant              Key Expression
    | Assignment            Expression Expression
    | ClassDef              MewlixClass
    | EnumDef               MewlixEnum
    | SuperCall             Arguments
    | ImportModule          ModuleData
    | ImportList            ModuleData [Key]
    | Return                Expression
    | Assert                Expression SourcePos
    | ThrowError            Expression SourcePos
    | TryCatch              Block (Maybe Key) Block
    | Rethrow
    | Break 
    | Continue
    deriving (Eq, Show)

data YarnBall = YarnBall
    { yarnballKey   :: Maybe ModuleKey
    , yarnballBlock :: Block          }
    deriving (Eq, Show)
