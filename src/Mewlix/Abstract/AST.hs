{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Abstract.AST 
( Primitive(..)
, Block(..)
, Params(..)
, Arguments(..)
, Expression(..)
, NumericOperationType(..)
, BinaryOperationType(..)
, UnaryOperationType(..)
, Statement(..)
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
      MInt Int
    | MString Text
    | MFloat Double
    | MBool Bool
    | MNil
    | Home
    | Outside
    deriving (Eq, Show)

data Expression =
    -- Bindings
      PrimitiveExpression   Primitive
    | Identifier            Key
    | ObjectProperty        Key
    -- Boolean Logic
    | BooleanAnd            Expression Expression
    | BooleanOr             Expression Expression
    -- Strings
    | StringCoerce          Expression
    | StringConcat          Expression Expression
    -- Numbers
    | NumberCoerce          Expression
    | NumericOperation      NumericOperationType Expression Expression
    -- Basic Operations
    | BinaryOperation       BinaryOperationType Expression Expression
    | UnaryOperation        UnaryOperationType Expression
    | TernaryOperation      Expression Expression Expression
    | ClawEntries           Expression
    -- Literals
    | ShelfExpression       [Expression]
    | BoxExpression         [(Key, Expression)]
    -- Reflection
    | AskType               Expression
    | IsInstance            Expression Expression
    -- Functions
    | LambdaExpression      Params Expression
    | FunctionCall          Expression Arguments
    -- Clowders
    | ClowderCreate         Expression Arguments
    -- Lookup
    | DotExpression         Expression Expression
    | LookupExpression      Expression Expression
    -- I/O
    | MeowExpression        Expression
    deriving (Eq, Show)

data NumericOperationType =
      Addition 
    | Subtraction
    | Multiplication
    | Division
    | FloorDivision
    | Modulo
    | Power
    deriving (Eq, Ord, Enum, Bounded, Show)

data BinaryOperationType =
    -- Shelf
      ShelfPush
    | Contains
    -- Comparison
    | Equal
    | LessThan
    | GreaterThan
    | NotEqual
    | GreaterOrEqual
    | LesserOrEqual
    deriving (Eq, Ord, Enum, Bounded, Show)

data UnaryOperationType =
    -- Shelf
      LengthLookup
    | ShelfPop
    | ShelfPeek
    -- Boolean
    | BooleanNot
    deriving (Eq, Ord, Enum, Bounded, Show)

newtype Block     = Block     { getBlock      :: [Statement]  } deriving (Eq, Show, Semigroup, Monoid)
newtype Params    = Params    { getParams     :: [Key]        } deriving (Eq, Show, Semigroup, Monoid)
newtype Arguments = Arguments { getArguments  :: [Expression] } deriving (Eq, Show, Semigroup, Monoid)

data MewlixFunction = MewlixFunction
    { functionName   :: Key
    , functionParams :: Params
    , functionBody   :: Block  }
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
    -- Expressions
      ExpressionStatement   Expression
    -- Control Flow
    | WhileLoop             Expression Block
    | ForEachLoop           Expression Key Block
    | IfElse                (NonEmpty Conditional) (Maybe Block)
    | Break 
    | Continue
    -- Functions
    | FunctionDef           MewlixFunction
    | FunctionAssignment    Expression MewlixFunction
    | Return                Expression
    -- Bindings
    | Variable              Key Expression
    | Constant              Key Expression
    | Assignment            Expression Expression
    -- Collections
    | ClassDef              MewlixClass
    | EnumDef               MewlixEnum
    | SuperCall             Arguments
    -- Modules
    | ImportModule          ModuleData
    | ImportList            ModuleData [Key]
    -- Assorted
    | Assert                Expression SourcePos
    | ThrowError            Expression SourcePos
    | TryCatch              Block (Maybe Key) Block
    | Rethrow
    deriving (Eq, Show)

data YarnBall = YarnBall
    { yarnballKey   :: Maybe ModuleKey
    , yarnballBlock :: Block          }
    deriving (Eq, Show)
