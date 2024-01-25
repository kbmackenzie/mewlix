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
import Mewlix.Compiler.Create
    ( construct
    , wrap
    , asFunction
    , syncCall
    , asyncCall
    , assignment
    , lambdaFunc
    )
import Mewlix.Compiler.Operations (binaryOpFunc, unaryOpFunc)
import qualified Mewlix.Compiler.Constants as Mewlix
import Lens.Micro.Platform ((.~), view)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List

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
    ----------------------------------------------
    transpileJS _ (Identifier key) = return key
    transpileJS _ (ObjectProperty key) = return key

    -- Lists + boxes:
    ----------------------------------------------
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
    ----------------------------------------------
    transpileJS _ (BooleanAnd left right) = do
        a  <- toJS left
        fb <- asFunction <$> toJS right
        wrap $ syncCall (Mewlix.operation "and") [ a, fb ]

    transpileJS _ (BooleanOr left right) = do
        a  <- toJS left
        fb <- asFunction <$> toJS right
        wrap $ syncCall (Mewlix.operation "or") [ a, fb ]

    -- Ternary operator:
    ----------------------------------------------
    transpileJS _ (TernaryOperation conditionExpr left right) = do
        condition <- toJS conditionExpr
        fa <- asFunction <$> toJS left
        fb <- asFunction <$> toJS right
        wrap $ syncCall (Mewlix.operation "ternary") [ parens condition, fa, fb ]

    -- Assignment expression:
    ----------------------------------------------
    transpileJS _ (Assignment key expr) = do
        left  <- toJS key
        right <- toJS expr
        wrap $ assignment left right

    -- Lambda function:
    ----------------------------------------------
    transpileJS _ (LambdaExpression params expr) = do
        body <- toJS expr
        wrap $ lambdaFunc (getParams params) body

    -- List expressions:
    ----------------------------------------------
    transpileJS _ (ListPush itemExpr shelfExpr) = do
        item  <- toJS itemExpr
        shelf <- toJS shelfExpr
        wrap $ syncCall (Mewlix.operation "push") [ shelf, item ]

    transpileJS _ (ListPop shelfExpr) = do
        shelf <- toJS shelfExpr
        wrap $ syncCall (Mewlix.operation "pop") [ shelf ]

    -- Function calls:
    ----------------------------------------------
    transpileJS _ (FunctionCall expr argExprs) = do
        args <- mapM toJS argExprs
        func <- toJS expr
        wrap $ asyncCall func args

    -- Dot expression:
    ----------------------------------------------
    transpileJS _ (DotExpression objectExpr propertyExpr) = do
        object   <- toJS objectExpr
        property <- toJS propertyExpr
        (return . Text.concat) [ object, ".box.", property ]

    -- Lookup expression:
    ----------------------------------------------
    transpileJS _ (LookupExpression objectExpr propertyExpr) = do
        let stringify = syncCall Mewlix.purrify . List.singleton
        object   <- toJS objectExpr
        property <- stringify <$> toJS propertyExpr
        (return . Text.concat) [ object, ".box", brackets property ]

    -- Binary operations:
    ----------------------------------------------
    transpileJS _ (BinaryOperation op left right) = do
        let func = binaryOpFunc op
        args <- mapM toJS [left, right]
        wrap $ func args

    -- Unary operations:
    ----------------------------------------------
    transpileJS _ (UnaryOperation op operand) = do
        let func = unaryOpFunc op
        arg  <- toJS operand
        wrap $ func [arg]

    -- 'Paw at' / Type of:
    ----------------------------------------------
    transpileJS _ (PawType operand) = do
        arg <- toJS operand
        wrap $ syncCall (Mewlix.operation "typeOf") [arg]

    -- 'Claw at'/ Box entries:
    ----------------------------------------------
    transpileJS _ (ClawEntries operand) = do
        arg <- toJS operand
        wrap $ syncCall (Mewlix.operation "pairs") [arg]
