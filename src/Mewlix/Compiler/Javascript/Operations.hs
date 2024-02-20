{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Javascript.Operations
( OperationBuilder
, binaryOpFunc
, unaryOpFunc
) where

import Data.Text (Text)
import Mewlix.Compiler.Javascript.ExpressionUtils (syncCall)
import Mewlix.String.Utils (sepComma)
import qualified Mewlix.Compiler.Javascript.Constants as Mewlix
import Mewlix.Abstract.AST (BinaryOp(..), UnaryOp(..))

type OperationBuilder = ([Text] -> Text)

compareTo :: [Text] -> OperationBuilder
compareTo comparisons = do
    let call = syncCall (Mewlix.compare "compare")
    let isOneOf = ".isOneOf(" <> sepComma comparisons <> ")"
    \args -> call args <> isOneOf

binaryOpFunc :: BinaryOp -> OperationBuilder
binaryOpFunc op = case op of 
    Addition        -> syncCall (Mewlix.arithmetic "add")
    Subtraction     -> syncCall (Mewlix.arithmetic "sub")
    Multiplication  -> syncCall (Mewlix.arithmetic "mul")
    Division        -> syncCall (Mewlix.arithmetic "div")
    Modulo          -> syncCall (Mewlix.arithmetic "mod")
    Power           -> syncCall (Mewlix.arithmetic "pow")
    ListConcat      -> syncCall (Mewlix.shelf "concat")
    Equal           -> syncCall (Mewlix.compare "isEqual")
    NotEqual        -> ("!" <>) . syncCall (Mewlix.compare"isEqual")
    LessThan        -> compareTo [Mewlix.lessThan] 
    GreaterThan     -> compareTo [Mewlix.greaterThan] 
    LesserOrEqual   -> compareTo [Mewlix.lessThan, Mewlix.equalTo] 
    GreaterOrEqual  -> compareTo [Mewlix.greaterThan, Mewlix.equalTo]

unaryOpFunc :: UnaryOp -> OperationBuilder
unaryOpFunc op = case op of
    Negation        -> syncCall (Mewlix.arithmetic  "negate")
    ListPeek        -> syncCall (Mewlix.shelf       "peek"  )
    BooleanNot      -> syncCall (Mewlix.boolean     "not"   )
    LengthLookup    -> syncCall (Mewlix.shelf       "length")
