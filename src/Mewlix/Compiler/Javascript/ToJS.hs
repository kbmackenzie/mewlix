{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Javascript.ToJS
( ToJS(..)
) where

import Mewlix.Abstract.AST
import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Abstract.Module (Module(..), hasAlias, defaultName)
import Mewlix.String.Escape (escapeString)
import Mewlix.String.Utils (parens, quotes, brackets, sepComma)
import Mewlix.Compiler.Javascript.Transpiler
import Mewlix.Utils.Show (showT)
import Mewlix.Compiler.Javascript.Expression
    ( instantiate
    , wrap
    , funcWrap
    , syncCall
    , asyncCall
    )
import Mewlix.Compiler.Javascript.Error
    ( ErrorCode(..)
    , createErrorIIFE
    )
import Mewlix.Compiler.Javascript.Statement
    ( terminate
    )
import Mewlix.Compiler.Javascript.Operations (binaryOpFunc, unaryOpFunc)
import qualified Mewlix.Compiler.Javascript.Constants as Mewlix
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)

type IndentLevel = Int;

class ToJS a where
    transpileJS :: IndentLevel -> a -> Transpiler Text

    toJS :: a -> Transpiler Text
    toJS = transpileJS 0

{- Primitives -}
-----------------------------------------------------------------
instance ToJS Primitive where
    transpileJS _ (MewlixInt i)     = (return . parens . showT) i
    transpileJS _ (MewlixBool b)    = (return . parens . showT) b
    transpileJS _ (MewlixFloat f)   = (return . parens . showT) f
    transpileJS _ (MewlixString s)  = (return . parens . quotes . escapeString) s
    transpileJS _ MewlixNil         = return "(null)"
    transpileJS _ MewlixHome        = return "this"
    transpileJS _ MewlixSuper       = return "super"

{- Expression -}
-----------------------------------------------------------------
instance ToJS Expression where
    transpileJS _ (PrimitiveExpr prim) = toJS prim

    -- Names:
    ----------------------------------------------
    transpileJS _ (Identifier key) = (return . getKey) key
    transpileJS _ (ObjectProperty key) = (return . getKey) key

    -- Lists + boxes:
    ----------------------------------------------
    transpileJS _ (ListExpression exprs) = do
        items <- mapM toJS exprs
        let array = (brackets . sepComma) items
        wrap $ instantiate Mewlix.createStack [ array ]

    transpileJS _ (BoxExpression pairs) = do
        let makeTuple :: (Key, Expression) -> Transpiler Text
            makeTuple (key, expr) = do
                value <- toJS expr
                (return . brackets) (getKey key <> ", " <> value)

        items <- mapM makeTuple pairs
        let array = (brackets . sepComma) items
        wrap $ instantiate Mewlix.mewlixBox [ array ]

    -- Boolean operations:
    ----------------------------------------------
    transpileJS _ (BooleanAnd left right) = do
        a  <- toJS left
        fb <- funcWrap <$> toJS right
        wrap $ syncCall (Mewlix.operation "and") [ a, fb ]

    transpileJS _ (BooleanOr left right) = do
        a  <- toJS left
        fb <- funcWrap <$> toJS right
        wrap $ syncCall (Mewlix.operation "or") [ a, fb ]

    -- Ternary operator:
    ----------------------------------------------
    transpileJS _ (TernaryOperation conditionExpr left right) = do
        condition <- toJS conditionExpr
        fa <- funcWrap <$> toJS left
        fb <- funcWrap <$> toJS right
        wrap $ syncCall (Mewlix.operation "ternary") [ parens condition, fa, fb ]

    -- Assignment expression:
    ----------------------------------------------
    transpileJS _ (Assignment key expr) = do
        left  <- toJS key
        right <- toJS expr
        wrap (left <> " = " <> right)

    -- Lambda function:
    ----------------------------------------------
    transpileJS _ (LambdaExpression paramExprs bodyExpr) = do
        body   <- toJS bodyExpr
        params <- toJS paramExprs
        wrap (params <> " => " <> body)

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
        args <- toJS argExprs
        func <- toJS expr
        return ("await " <> func <> args)

    -- Dot expression:
    ----------------------------------------------
    transpileJS _ (DotExpression objectExpr propertyExpr) = do
        object   <- toJS objectExpr
        property <- toJS propertyExpr
        return (object <> ".box." <> property)

    -- Lookup expression:
    ----------------------------------------------
    transpileJS _ (LookupExpression objectExpr propertyExpr) = do
        let stringify = syncCall Mewlix.purrify . List.singleton
        object   <- toJS objectExpr
        property <- stringify <$> toJS propertyExpr
        return (object <> ".box" <> brackets property)

    -- Clowder expressions:
    ----------------------------------------------
    transpileJS _ (ClowderCreate clowderExpr argExprs) = do
        clowder <- toJS clowderExpr
        args    <- toJS argExprs
        return ("await new " <> clowder <> "().wake" <> args)

    transpileJS _ (SuperCall argExprs) = do
        args    <- toJS argExprs
        return ("super.wake" <> args)

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

    -- 'Throw' expression:
    ----------------------------------------------
    transpileJS _ (ThrowError expr pos) = do
        arg <- toJS expr
        return $ createErrorIIFE CatOnComputer pos arg

    -- IO:
    ----------------------------------------------
    transpileJS _ (MeowExpression expr) = do
        arg <- toJS expr
        wrap $ asyncCall Mewlix.meow [arg]

    transpileJS _ (ListenExpression expr) = do
        arg <- toJS expr
        wrap $ asyncCall Mewlix.listen [arg]

{- Params -}
-----------------------------------------------------------------
instance ToJS Params where
    transpileJS _ (Params params) = (wrap . sepComma . map getKey) params

{- Arguments -}
-----------------------------------------------------------------
instance ToJS Arguments where
    transpileJS _ (Arguments argExprs) = do
        args <- mapM toJS argExprs
        wrap $ sepComma args

{- Statements -}
-----------------------------------------------------------------
-- Note: Every statement should be transpiled with no trailing linebreaks.
-- All linebreaks between statements will be added somewhere else.

instance ToJS Statement where
    -- Expressions:
    transpileJS _       (ExpressionStatement expr) = terminate <$> toJS expr

    -- Control flow:
    transpileJS level   (IfElse conditionals else_) = do

        let transpileConditional :: Conditional -> Transpiler Text
            transpileConditional (Conditional expr block) = do
                condition <- toJS expr
                body      <- transpileJS level block
                let header = mconcat [ "if (", condition, ") " ]
                return (header <> body)

        initialCondition     <- transpileConditional (NonEmpty.head conditionals)
        additionalConditions <- mapM transpileConditional (NonEmpty.tail conditionals)

        elseBlock <- case else_ of
            Nothing      -> return Text.empty
            (Just block) -> do
                body <- transpileJS level block
                return ("else " <> body)

        return $ Text.unlines
            [ initialCondition
            , (Text.unlines . map ("else " <>)) additionalConditions
            , elseBlock                                             ]

    -- While loop:
    transpileJS level   (WhileLoop expr block) = do
        condition   <- toJS expr
        body        <- transpileJS level block
        let header = mconcat [ "while (", condition, ") " ]
        return (header <> body)

    -- Foreach loop:
    transpileJS level   (ForEachLoop expr key block) = do
        iterable    <- toJS expr
        body        <- transpileJS level block
        let header = mconcat [ "for (const ", getKey key, " of ", iterable, ") " ]
        return (header <> body)

    -- Bindings:
    transpileJS _       (Binding key expr) = do
        value       <- toJS expr
        return $ mconcat [ "let ", getKey key, " = ", value, ";" ]

    transpileJS _       (LocalBinding key expr) = toJS (Binding key expr)

    -- Loop keywords:
    transpileJS _       Break = return "break;"
    transpileJS _       Continue = return "continue;"

    -- Return keyword:
    transpileJS _       (Return expr) = do
        value <- toJS expr
        return $ mconcat [ "return ", value, ";" ]

    -- Import statement:
    transpileJS _       (ImportStatement module_) = do
        let key = maybe (defaultName module_) getKey (moduleAlias module_)
        let value = asyncCall Mewlix.getModule [key]
        return $ mconcat [ "const ", key, " = ", value, ";" ]

{- Block -}
-----------------------------------------------------------------
instance ToJS Block where
    transpileJS level (Block statements) = do
        let newLevel = succ level
        transpiledStatements <- mapM (transpileJS newLevel) statements
        return $ mconcat [ "{\n", Text.unlines transpiledStatements, "\n}" ]
