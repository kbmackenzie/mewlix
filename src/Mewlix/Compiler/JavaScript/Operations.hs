{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.Operations
( OperationBuilder
, binaryOpFunc
, unaryOpFunc
) where

import Data.Text (Text)
import Mewlix.Compiler.JavaScript.Utils.Expression (call)
import Mewlix.String.Utils (sepComma)
import qualified Mewlix.Compiler.JavaScript.Constants as Mewlix
import Mewlix.Abstract.AST (BinaryOp(..), UnaryOp(..))

type OperationBuilder = ([Text] -> Text)

ordering :: [Text] -> OperationBuilder
ordering comparisons = do
    let comparer = call (Mewlix.compare "ordering")
    let isOneOf = ".isOneOf(" <> sepComma comparisons <> ")"
    \args -> comparer args <> isOneOf

binaryOpFunc :: BinaryOp -> OperationBuilder
binaryOpFunc op = case op of 
    Addition        -> call (Mewlix.numbers "add")
    Subtraction     -> call (Mewlix.numbers "sub")
    Multiplication  -> call (Mewlix.numbers "mul")
    Division        -> call (Mewlix.numbers "div")
    FloorDivision   -> call (Mewlix.numbers "floordiv")
    Modulo          -> call (Mewlix.numbers "mod")
    Power           -> call (Mewlix.numbers "pow")
    ListPush        -> call (Mewlix.shelves "push")
    StringConcat    -> call (Mewlix.strings "concat")
    Contains        -> call (Mewlix.shelves "contains")
    Equal           -> call (Mewlix.compare "equal")
    NotEqual        -> ("!" <>) . call (Mewlix.compare "equal")
    LessThan        -> ordering [Mewlix.lessThan]
    GreaterThan     -> ordering [Mewlix.greaterThan]
    LesserOrEqual   -> ordering [Mewlix.lessThan, Mewlix.equalTo]
    GreaterOrEqual  -> ordering [Mewlix.greaterThan, Mewlix.equalTo]

unaryOpFunc :: UnaryOp -> OperationBuilder
unaryOpFunc op = case op of
    Minus           -> call (Mewlix.numbers "minus" )
    Plus            -> call (Mewlix.numbers "plus"  )
    ListPop         -> call (Mewlix.shelves "pop"   )
    ListPeek        -> call (Mewlix.shelves "peek"  )
    BooleanNot      -> call (Mewlix.boolean "not"   )
    LengthLookup    -> call (Mewlix.shelves "length")
