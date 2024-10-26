{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.Operations
( OperationBuilder
, binaryOpFunc
, unaryOpFunc
) where

import Data.Text (Text)
import Mewlix.Compiler.JavaScript.Utils (call)
import qualified Mewlix.Compiler.JavaScript.Constants as Mewlix
import Mewlix.Abstract.AST (BinaryOp(..), UnaryOp(..))

type OperationBuilder = ([Text] -> Text)

ordering :: Text -> OperationBuilder
ordering comparison = do
    let orderer  = call (Mewlix.relation "ordering")
    let comparer = call (Mewlix.compare comparison)
    \args -> comparer [orderer args]

binaryOpFunc :: BinaryOp -> OperationBuilder
binaryOpFunc op = case op of 
    Addition        -> call (Mewlix.numbers  "add"      )
    Subtraction     -> call (Mewlix.numbers  "sub"      )
    Multiplication  -> call (Mewlix.numbers  "mul"      )
    Division        -> call (Mewlix.numbers  "div"      )
    FloorDivision   -> call (Mewlix.numbers  "floordiv" )
    Modulo          -> call (Mewlix.numbers  "mod"      )
    Power           -> call (Mewlix.numbers  "pow"      )
    ListPush        -> call (Mewlix.shelf    "push"     )
    StringConcat    -> call (Mewlix.strings  "concat"   )
    Contains        -> call (Mewlix.shelf    "contains" )
    Equal           -> call (Mewlix.relation "equal"    )
    NotEqual        -> ("!" <>) . call (Mewlix.relation "equal")
    LessThan        -> ordering "less"
    GreaterThan     -> ordering "greater"
    LesserOrEqual   -> ordering "lessOrEqual"
    GreaterOrEqual  -> ordering "greaterOrEqual"

unaryOpFunc :: UnaryOp -> OperationBuilder
unaryOpFunc op = case op of
    Minus           -> call (Mewlix.numbers "minus" )
    Plus            -> call (Mewlix.numbers "plus"  )
    ListPop         -> call (Mewlix.shelf   "pop"   )
    ListPeek        -> call (Mewlix.shelf   "peek"  )
    BooleanNot      -> call (Mewlix.boolean "not"   )
    LengthLookup    -> call (Mewlix.collections "length")
