{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.Operations
( OperationBuilder
, binaryOpFunc
, unaryOpFunc
) where

import Data.Text (Text)
import Mewlix.Compiler.JavaScript.Utils.Expression (syncCall)
import Mewlix.String.Utils (sepComma)
import qualified Mewlix.Compiler.JavaScript.Constants as Mewlix
import Mewlix.Abstract.AST (BinaryOp(..), UnaryOp(..))

type OperationBuilder = ([Text] -> Text)

compareTo :: [Text] -> OperationBuilder
compareTo comparisons = do
    let call = syncCall (Mewlix.compare "compare")
    let isOneOf = ".isOneOf(" <> sepComma comparisons <> ")"
    \args -> call args <> isOneOf

binaryOpFunc :: BinaryOp -> OperationBuilder
binaryOpFunc op = case op of 
    Addition        -> syncCall (Mewlix.numbers "add")
    Subtraction     -> syncCall (Mewlix.numbers "sub")
    Multiplication  -> syncCall (Mewlix.numbers "mul")
    Division        -> syncCall (Mewlix.numbers "div")
    FloorDivision   -> syncCall (Mewlix.numbers "floordiv")
    Modulo          -> syncCall (Mewlix.numbers "mod")
    Power           -> syncCall (Mewlix.numbers "pow")
    ListPush        -> syncCall (Mewlix.shelves "push")
    StringConcat    -> syncCall (Mewlix.strings "concat")
    Contains        -> syncCall (Mewlix.shelves "contains")
    Equal           -> syncCall (Mewlix.compare "isEqual")
    NotEqual        -> ("!" <>) . syncCall (Mewlix.compare "isEqual")
    LessThan        -> compareTo [Mewlix.lessThan] 
    GreaterThan     -> compareTo [Mewlix.greaterThan] 
    LesserOrEqual   -> compareTo [Mewlix.lessThan, Mewlix.equalTo] 
    GreaterOrEqual  -> compareTo [Mewlix.greaterThan, Mewlix.equalTo]

unaryOpFunc :: UnaryOp -> OperationBuilder
unaryOpFunc op = case op of
    Negation        -> syncCall (Mewlix.numbers "minus" )
    Plus            -> syncCall (Mewlix.numbers "plus"  )
    ListPop         -> syncCall (Mewlix.shelves "pop"   )
    ListPeek        -> syncCall (Mewlix.shelves "peek"  )
    BooleanNot      -> syncCall (Mewlix.boolean "not"   )
    LengthLookup    -> syncCall (Mewlix.shelves "length")
