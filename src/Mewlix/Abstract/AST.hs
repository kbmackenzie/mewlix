{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Abstract.AST 
( Primitive(..)
, Key
, Block(..)
, Params(..)
, Expression(..)
, Statement(..)
, BinaryOp(..)
, UnaryOp(..)
, MewlixFunction(..)
, MewlixClass(..)
, LiftedExpression(..)
) where

import Mewlix.Data.Key (Key)
import Data.Text (Text)
import Mewlix.Utils.Types

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
    | Increment             Expression
    | Decrement             Expression
    | ListPush              Expression Expression
    | ListPop               Expression
    | LambdaExpression      Params Expression
    | FunctionCall          [Expression] Expression
    | DotExpression         Expression Expression
    | LookupExpression      Expression Expression
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
    deriving (Show)

data UnaryOp =
      Negation
    | ListPeek
    | BooleanNot
    | LengthLookup
    deriving (Show)

newtype Block  = Block  [Statement] deriving (Show, Semigroup, Monoid)
newtype Params = Params [Text]      deriving (Show, Semigroup, Monoid)

type CatchBlock = (Maybe Expression, Block)

{- A 'lifted' expression type that allows declarations.
 - It's compiled into an IIFE if needed.                -}
data LiftedExpression =
      LiftExpression  Expression
    | LiftDeclaration Key Expression
    deriving (Show)

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

data Statement =
      ExpressionStatement   Expression
    | WhileLoop             Expression Block
    | ForLoop               (LiftedExpression, Expression, Expression) Block
    | IfElse                Expression Block Block
    | FunctionDef           MewlixFunction
    | Declaration           Key Expression
    | ClassDef              MewlixClass
    | ImportStatement       FilePathT (Maybe Key)
    | Return                Expression
    | TryCatch              Block CatchBlock
    | Break 
    | Continue
    deriving (Show)

