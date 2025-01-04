module Mewlix.Compiler.Analysis
( Operation(..)
, AnalyzeOperation(..)
) where

import Mewlix.Abstract.AST
    ( Expression(..)
    , Block(..)
    , Statement(..)
    , Arguments(..)
    , Conditional(..)
    )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

data Operation = Or | And | Ternary deriving (Eq, Ord, Enum, Bounded)

class AnalyzeOperation a where
    analyze :: a -> Set Operation

instance AnalyzeOperation Expression where
    analyze expr = case expr of
        (BooleanAnd a b)         -> Set.singleton And <> analyze a <> analyze b
        (BooleanOr a b)          -> Set.singleton Or  <> analyze a <> analyze b
        (TernaryOperation a b c) -> Set.singleton Ternary <> analyze a <> analyze b <> analyze c
        (BinaryOperation _ a b)  -> analyze a <> analyze b
        (UnaryOperation _ a)     -> analyze a
        (ShelfExpression xs)     -> mconcat . map analyze $ xs
        (BoxExpression xs)       -> mconcat . map (analyze . snd) $ xs
        (AskType a)              -> analyze a
        (IsInstance a b)         -> analyze a <> analyze b
        (ClawEntries a)          -> analyze a
        (FunctionCall func args) -> analyze func <> analyze args
        (ClowderCreate cl args)  -> analyze cl <> analyze args
        (DotExpression a b)      -> analyze a <> analyze b
        (LookupExpression a b)   -> analyze a <> analyze b
        (MeowExpression a)       -> analyze a
        (LambdaExpression _ _)   -> mempty
        _                        -> mempty

instance AnalyzeOperation Statement where
    analyze stmt = case stmt of
        (ExpressionStatement expr)  -> analyze expr
        (Variable _ expr)           -> analyze expr
        (Constant _ expr)           -> analyze expr
        (FunctionAssignment func _) -> analyze func
        (FunctionDef _)             -> mempty
        (Assignment a b)            -> analyze a <> analyze b
        (SuperCall args)            -> analyze args
        (Return expr)               -> analyze expr
        (Assert expr _)             -> analyze expr
        (ThrowError expr _)         -> analyze expr
        (TryCatch trys _ catchs)    -> analyze trys <> analyze catchs
        (WhileLoop cond block)      -> analyze cond <> analyze block
        (ForEachLoop iter _ block)  -> analyze iter <> analyze block
        (IfElse ifs elses)          -> analyze ifs <> maybe mempty analyze elses
        _                           -> mempty

instance AnalyzeOperation Block where
    analyze = mconcat . map analyze . getBlock

instance AnalyzeOperation Arguments where
    analyze = mconcat . map analyze . getArguments

instance AnalyzeOperation Conditional where
    analyze (Conditional condition block) = analyze condition <> analyze block

instance (AnalyzeOperation a) => AnalyzeOperation (NonEmpty a) where
    analyze = mconcat . map analyze . NonEmpty.toList
