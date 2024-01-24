{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Create
( construct
, wrap
, asFunction
, syncCall
, asyncCall
, binaryOp
, lambdaFunc
, OperationBuilder
, binaryOpFunc
, unaryOpFunc
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.String.Utils ((|++), sepComma, parens)
import qualified Mewlix.Compiler.Constants as Mewlix
import Mewlix.Abstract.AST (BinaryOp(..), UnaryOp(..))

construct :: Text -> [Text] -> Text
construct name args = Text.concat [ "new ", name, "(", sepComma args, ")" ]

wrap :: (Monad m) => Text -> m Text
wrap = return . parens

asFunction :: Text -> Text
asFunction body = Text.concat [ "(() => ", body, ")" ]

syncCall :: Text -> [Text] -> Text
syncCall name args = Text.concat [ name, "(", sepComma args, ")" ]

asyncCall :: Text -> [Text] -> Text
asyncCall name args = Text.concat [ "await ", name, "(", sepComma args, ")" ]

binaryOp :: Text -> Text -> Text -> Text
binaryOp operator a b = Text.intercalate " " [ a, operator, b ]

lambdaFunc :: [Text] -> Text -> Text
lambdaFunc params body = Text.concat [ "(", sepComma params, ") => ", body ]

compareTo :: [Text] -> OperationBuilder
compareTo comparisons = do
    let call = syncCall (Mewlix.operation "compare")
    let isOneOf = Text.concat [ ".isOneOf(", sepComma comparisons, ")" ]
    \args -> call args |++ isOneOf

type OperationBuilder = ([Text] -> Text)

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
