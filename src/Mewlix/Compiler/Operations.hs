{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Operations
( OperationBuilder
, binaryOpFunc
, unaryOpFunc
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.Compiler.Create (syncCall)
import Mewlix.String.Utils ((|++), sepComma)
import qualified Mewlix.Compiler.Constants as Mewlix
import Mewlix.Abstract.AST (BinaryOp(..), UnaryOp(..))

type OperationBuilder = ([Text] -> Text)

compareTo :: [Text] -> OperationBuilder
compareTo comparisons = do
    let call = syncCall (Mewlix.operation "compare")
    let isOneOf = Text.concat [ ".isOneOf(", sepComma comparisons, ")" ]
    \args -> call args |++ isOneOf

binaryOpFunc :: BinaryOp -> OperationBuilder
binaryOpFunc op = case op of 
    Addition        -> syncCall (Mewlix.operation "add")
    Subtraction     -> syncCall (Mewlix.operation "sub")
    Multiplication  -> syncCall (Mewlix.operation "mul")
    Division        -> syncCall (Mewlix.operation "div")
    Modulo          -> syncCall (Mewlix.operation "mod")
    Power           -> syncCall (Mewlix.operation "pow")
    ListConcat      -> syncCall (Mewlix.operation "concat")
    Equal           -> syncCall (Mewlix.operation "isEqual")
    NotEqual        -> ("!" |++) . syncCall (Mewlix.operation "isEqual")
    LessThan        -> compareTo [Mewlix.lessThan] 
    GreaterThan     -> compareTo [Mewlix.greaterThan] 
    LesserOrEqual   -> compareTo [Mewlix.lessThan, Mewlix.equalTo] 
    GreaterOrEqual  -> compareTo [Mewlix.greaterThan, Mewlix.equalTo]

unaryOpFunc :: UnaryOp -> OperationBuilder
unaryOpFunc op = case op of
    Negation        -> syncCall (Mewlix.operation "negate")
    ListPeek        -> syncCall (Mewlix.operation "peek")
    BooleanNot      -> syncCall (Mewlix.operation "not")
    LengthLookup    -> syncCall (Mewlix.operation "length")
