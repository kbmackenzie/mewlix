{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.ToJS
( ToJS(..)
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.String.Escape (escapeString)
import Mewlix.String.Utils ((|++), parens, quotes, brackets, sepComma)
import Mewlix.Abstract.AST
import Mewlix.Compiler.Transpiler
import Mewlix.Utils.Show (showT)
import Mewlix.Compiler.Create (construct, wrap, asFunction, syncCall, binaryOp)
import qualified Mewlix.Compiler.Constants as Mewlix
import Lens.Micro.Platform ((.~), view)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

type IndentLevel = Int;

class ToJS a where
    transpileJS :: IndentLevel -> a -> Transpiler Text

    toJS :: a -> Transpiler Text
    toJS = transpileJS 0

instance ToJS Primitive where
    transpileJS _ (MewlixInt i)     = (return . parens . showT) i
    transpileJS _ (MewlixBool b)    = (return . parens . showT) b
    transpileJS _ (MewlixFloat f)   = (return . parens . showT) f
    transpileJS _ (MewlixString s)  = (return . parens . quotes . escapeString) s
    transpileJS _ MewlixNil         = return "(null)"
    transpileJS _ MewlixHome        = return "this"
    transpileJS _ MewlixSuper       = return "super"

instance ToJS Expression where
    transpileJS _ (PrimitiveExpr prim) = toJS prim

    -- Names:
    transpileJS _ (Identifier key) = return key
    transpileJS _ (ObjectProperty key) = return (".box." |++ key)

    -- Lists:
    transpileJS _ (ListExpression exprs) = do
        items <- mapM toJS exprs
        let array = (brackets . sepComma) items
        wrap $ construct Mewlix.createStack [ array ]

    transpileJS _ (BoxExpression pairs) = do
        let makeTuple :: (Key, Expression) -> Transpiler Text
            makeTuple (key, expr) = do
                value <- toJS expr
                (return . brackets . Text.concat) [ key, ", ", value ]

        items <- mapM makeTuple pairs
        let array = (brackets . sepComma) items
        wrap $ construct Mewlix.mewlixBox [ array ]

    -- Boolean operations:
    transpileJS _ (BooleanAnd a b) = do
        fa <- asFunction <$> toJS a
        fb <- asFunction <$> toJS b
        wrap $ syncCall (Mewlix.operation "and") [ fa, fb ]

    transpileJS _ (BooleanOr a b) = do
        fa <- asFunction <$> toJS a
        fb <- asFunction <$> toJS b
        wrap $ syncCall (Mewlix.operation "or") [ fa, fb ]

    -- Ternary operator:
    transpileJS _ (TernaryOperation condition a b) = do
        predicate <- toJS condition
        fa <- asFunction <$> toJS a
        fb <- asFunction <$> toJS b
        wrap $ syncCall (Mewlix.operation "ternary") [ parens predicate, fa, fb ]

    -- Assignment expression:
    transpileJS _ (Assignment key expr) = do
        left  <- toJS key
        right <- toJS expr
        wrap $ binaryOp "=" left right

    transpileJS _ _ = undefined

{-
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

data Module = Module
    { modulePath    :: FilePathT
    , moduleBlock   :: Block   }
    deriving (Show)
 -}
