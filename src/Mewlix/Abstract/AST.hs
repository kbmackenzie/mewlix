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
    | AskType               Expression
    | IsInstance            Expression Expression
    | ClawEntries           Expression
    | LambdaExpression      Params Expression
    | FunctionCall          Expression Arguments
    | ClowderCreate         Expression Arguments
    | DotExpression         Expression Expression
    | LookupExpression      Expression Expression
    | MeowExpression        Expression
    | ListenExpression      (Maybe Expression)
    deriving (Show)

data BinaryOp =
      Addition 
    | Subtraction
    | Multiplication
    | Division
    | FloorDivision
    | Modulo
    | Power
    | ListPush
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
      Negation
    | ListPop
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

data Conditional = Conditional
    { conditionalExpression :: Expression
    , conditionalBlock      :: Block             }
    deriving (Show)

data Statement =
      ExpressionStatement   Expression
    | WhileLoop             Expression Block
    | ForEachLoop           Expression Key Block
    | IfElse                (NonEmpty Conditional) (Maybe Block)
    | FunctionDef           MewlixFunction
    | Variable              Key Expression
    | Constant              Key Expression
    | ClassDef              MewlixClass
    | SuperCall             Arguments
    | ImportModule          ModuleData
    | ImportList            ModuleData [Key]
    | Return                Expression
    | Assert                Expression SourcePos
    | TryCatch              Block (Maybe Key) Block
    | ThrowError            Expression SourcePos
    | Break 
    | Continue
    deriving (Show)

data YarnBall = YarnBall
    { yarnballKey   :: Maybe ModuleKey
    , yarnballBlock :: Block          }
    deriving (Show)
