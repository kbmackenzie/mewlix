{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Javascript.ToJavascript
( ToJavascript(..)
) where

import Mewlix.Abstract.AST
    ( Primitive(..)
    , Block(..)
    , Params(..)
    , Arguments(..)
    , Expression(..)
    , Statement(..)
    , MewlixFunction(..)
    , MewlixClass(..)
    , Conditional(..)
    , YarnBall(..)
    ) 
import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Abstract.Module (ModuleData(..), joinKey, defaultName)
import Mewlix.String.Escape (escapeString)
import Mewlix.String.Utils (quotes, brackets, sepComma, separateLines)
import Mewlix.Compiler.Transpiler (TranspilerContext(..), Transpiler , asks)
import Mewlix.Utils.Show (showT)
import Mewlix.Compiler.Indentation
    ( Indentation
    , zeroIndent
    , toIndent
    , indentLine
    , indentMany
    )
import Mewlix.Compiler.Javascript.ExpressionUtils
    ( instantiate
    , wrap
    , lambda
    , asyncLambda
    , syncCall
    , asyncCall
    , asBoolean
    )
import Mewlix.Compiler.Javascript.ErrorUtils (ErrorCode(..), errorInfo, createError)
import Mewlix.Compiler.Javascript.StatementUtils (terminate, findBindings)
import Mewlix.Compiler.Javascript.Operations (binaryOpFunc, unaryOpFunc)
import qualified Mewlix.Compiler.Javascript.Constants as Mewlix
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.HashMap.Strict as HashMap
import Data.Char (isSpace)

class ToJavascript a where
    transpileJS :: Indentation -> a -> Transpiler Text

    toJS :: a -> Transpiler Text
    toJS = transpileJS zeroIndent

{- Primitives -}
-----------------------------------------------------------------
instance ToJavascript Primitive where
    transpileJS _ (MewlixInt i)     = (return . showT) i
    transpileJS _ (MewlixFloat f)   = (return . showT) f
    transpileJS _ (MewlixString s)  = (return . quotes . escapeString) s
    transpileJS _ (MewlixBool b)    = (return . Text.toLower . showT) b
    transpileJS _ MewlixNil         = return "null"
    transpileJS _ MewlixHome        = return "this"
    transpileJS _ MewlixSuper       = return "super"

{- Expression -}
-----------------------------------------------------------------
instance ToJavascript Expression where
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
        return $ syncCall Mewlix.createShelf [ array ]

    transpileJS _ (BoxExpression pairs) = do
        let makeKey :: Key -> Transpiler Text
            makeKey = toJS . MewlixString . getKey

        let makeTuple :: (Key, Expression) -> Transpiler Text
            makeTuple (key, expr) = do
                bind  <- makeKey key
                value <- toJS expr
                (return . brackets) (bind <> ", " <> value)

        items <- mapM makeTuple pairs
        let array = (brackets . sepComma) items
        return $ instantiate Mewlix.box [ array ]

    -- Boolean operations:
    ----------------------------------------------
    transpileJS _ (BooleanAnd left right) = do
        a  <- toJS left
        fb <- asyncLambda <$> toJS right
        return $ syncCall (Mewlix.boolean "and") [ a, fb ]

    transpileJS _ (BooleanOr left right) = do
        a  <- toJS left
        fb <- asyncLambda <$> toJS right
        return $ syncCall (Mewlix.boolean "or") [ a, fb ]

    -- Ternary operator:
    ----------------------------------------------
    transpileJS _ (TernaryOperation conditionExpr left right) = do
        condition <- toJS conditionExpr
        fa <- asyncLambda <$> toJS left
        fb <- asyncLambda <$> toJS right
        return $ syncCall (Mewlix.boolean "ternary") [ condition, fa, fb ]

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
        wrap ("async " <> params <> " => " <> body)

    -- List expressions:
    ----------------------------------------------
    transpileJS _ (ListPush itemExpr shelfExpr) = do
        item  <- toJS itemExpr
        shelf <- toJS shelfExpr
        return $ syncCall (Mewlix.shelves "push") [ shelf, item ]

    transpileJS _ (ListPop shelfExpr) = do
        shelf <- toJS shelfExpr
        return $ syncCall (Mewlix.shelves "pop") [ shelf ]

    -- Function calls:
    ----------------------------------------------
    transpileJS _ (FunctionCall expr argExprs) = do
        args <- toJS argExprs
        func <- toJS expr
        wrap ("await " <> func <> args)

    -- Dot expression:
    ----------------------------------------------
    transpileJS _ (DotExpression objectExpr propertyExpr) = do
        object   <- toJS objectExpr
        property <- toJS propertyExpr
        return (object <> ".box()." <> property)

    -- Lookup expression:
    ----------------------------------------------
    transpileJS _ (LookupExpression objectExpr propertyExpr) = do
        let stringify = syncCall Mewlix.purrify . List.singleton
        object   <- toJS objectExpr
        property <- stringify <$> toJS propertyExpr
        return (object <> ".box()" <> brackets property)

    -- Clowder expressions:
    ----------------------------------------------
    transpileJS _ (ClowderCreate clowderExpr argExprs) = do
        clowder <- toJS clowderExpr
        args    <- toJS argExprs
        wrap ("await new " <> clowder <> "().wake" <> args)

    transpileJS _ (SuperCall argExprs) = do
        args    <- toJS argExprs
        wrap ("await super.wake" <> args)

    -- Binary operations:
    ----------------------------------------------
    transpileJS _ (BinaryOperation op left right) = do
        let func = binaryOpFunc op
        args <- mapM toJS [left, right]
        return $ func args

    -- Unary operations:
    ----------------------------------------------
    transpileJS _ (UnaryOperation op operand) = do
        let func = unaryOpFunc op
        arg  <- toJS operand
        return $ func [arg]

    -- 'Paw at' / Type of:
    ----------------------------------------------
    transpileJS _ (PawType operand) = do
        arg <- toJS operand
        return $ syncCall (Mewlix.reflection "typeOf") [arg]

    -- 'Is' / Instance of:
    ----------------------------------------------
    transpileJS _ (IsInstance a b) = do
        args <- mapM toJS [a, b]
        return $ syncCall (Mewlix.reflection "instanceOf") args

    -- 'Claw at'/ Box entries:
    ----------------------------------------------
    transpileJS _ (ClawEntries operand) = do
        arg <- toJS operand
        return $ syncCall (Mewlix.boxes "pairs") [arg]

    -- 'Throw' expression:
    ----------------------------------------------
    transpileJS _ (ThrowError expr pos) = do
        arg <- toJS expr
        return $ createError CatOnComputer pos arg

    -- IO:
    ----------------------------------------------
    transpileJS _ (MeowExpression expr) = do
        arg <- toJS expr
        return $ asyncCall Mewlix.meow [arg]

    transpileJS _ (ListenExpression expr) = do
        arg <- toJS expr
        return $ asyncCall Mewlix.listen [arg]

{- Params -}
-----------------------------------------------------------------
instance ToJavascript Params where
    transpileJS _ (Params params) = (wrap . sepComma . map getKey) params

{- Arguments -}
-----------------------------------------------------------------
instance ToJavascript Arguments where
    transpileJS _ (Arguments argExprs) = do
        args <- mapM toJS argExprs
        wrap $ sepComma args

{- Statements -}
-----------------------------------------------------------------
-- Note: Every statement should be transpiled with no trailing linebreaks.
-- All linebreaks between statements will be added somewhere else.

instance ToJavascript Statement where
    -- Expressions:
    ----------------------------------------------
    transpileJS level       (ExpressionStatement expr) = do
        indentLine level . terminate <$> toJS expr

    -- Control flow:
    ----------------------------------------------
    transpileJS level   (IfElse conditionals else_) = do

        let transpileConditional :: Conditional -> Transpiler Text
            transpileConditional (Conditional expr block) = do
                condition <- asBoolean <$> toJS expr
                body      <- transpileJS level block
                let header = mconcat [ "if (", condition, ") " ]
                return (header <> body)

        initialConditional  <- transpileConditional (NonEmpty.head conditionals)
        moreConditionals    <- mapM transpileConditional (NonEmpty.tail conditionals)

        elseBlock <- case else_ of
            Nothing      -> return Text.empty
            (Just block) -> do
                body <- transpileJS level block
                return ("else " <> body)

        return . separateLines . filter (Text.any (not . isSpace)) $
            [ indentLine level initialConditional
            , (separateLines . indentMany level . map ("else " <>)) moreConditionals
            , indentLine level elseBlock                                            ]

    -- While loop:
    ----------------------------------------------
    transpileJS level   (WhileLoop expr block) = do
        condition   <- asBoolean <$> toJS expr
        body        <- transpileJS level block
        let header = indentLine level $ mconcat [ "while (", condition, ") " ]
        return (header <> body)

    -- Foreach loop:
    ----------------------------------------------
    transpileJS level   (ForEachLoop expr key block) = do
        iterable    <- toJS expr
        body        <- transpileJS level block

        let rainable = syncCall Mewlix.rainable [iterable]
        let header = indentLine level $ mconcat
                [ "for (const ", getKey key, " of ", rainable, ") "]

        return (header <> body)

    -- Bindings:
    ----------------------------------------------
    transpileJS level    (Binding key expr) = do
        value <- toJS expr
        let declaration = mconcat [ "let ", getKey key, " = ", value, ";" ]
        return (indentLine level declaration)
    transpileJS level    (LocalBinding key expr) = transpileJS level (Binding key expr)

    -- Functions:
    ----------------------------------------------
    transpileJS level   (FunctionDef func) = do
        funcExpr <- transpileJS level func
        let declaration = mconcat [ "const ", (getKey . funcName) func, " = ", funcExpr, ";" ]
        return (indentLine level declaration)

    -- Loop keywords:
    ----------------------------------------------
    transpileJS level   Break = return (indentLine level "break;")
    transpileJS level   Continue = return (indentLine level "continue;")

    -- Return keyword:
    ----------------------------------------------
    transpileJS level   (Return expr) = do
        value <- toJS expr
        let ret = mconcat [ "return ", value, ";" ]
        return (indentLine level ret)

    -- Import statement:
    ----------------------------------------------
    transpileJS level   (ImportModule moduleData) = do
        let binding = maybe (defaultName moduleData) getKey (moduleAlias moduleData)
        importValue <- do
            let key = (Key . joinKey . moduleKey) moduleData
            stringKey <- (toJS . MewlixString . getKey) key
            special   <- asks specialImports
            case HashMap.lookup key special of
                Nothing      -> return $ asyncCall Mewlix.getModule [stringKey]
                (Just value) -> return $ syncCall Mewlix.wrap [value]
        let declaration = mconcat [ "const ", binding, " = ", importValue, ";" ]
        return (indentLine level declaration)

    transpileJS level   (ImportList moduleData keys) = do
        importValue <- do
            let key = (Key . joinKey . moduleKey) moduleData
            stringKey <- (toJS . MewlixString . getKey) key
            special   <- asks specialImports
            case HashMap.lookup key special of
                Nothing      -> return $ asyncCall Mewlix.getModule [stringKey]
                (Just value) -> return $ syncCall Mewlix.wrap [value]

        let bind :: Key -> Text
            bind key = indentLine level $ mconcat
                [ "const ", getKey key, " = ", importValue, ";" ]

        (return . separateLines) (map bind keys)

    -- Class statement:
    ----------------------------------------------
    transpileJS level   (ClassDef clowder) = do
        let extends = maybe Mewlix.clowder getKey (classExtends clowder)
        let header = mconcat [ "class ", (getKey . className) clowder, " extends ", extends ]

        let classLevel  = succ level
        let methodLevel = succ classLevel

        let transpileMethod :: MewlixFunction -> Transpiler Text
            transpileMethod func = do
                funcExpr <- transpileJS methodLevel func
                return $ mconcat [ "this.", (getKey . funcName) func, " = ", funcExpr, ";" ]

        methods <- mapM transpileMethod (classMethods clowder)
        let constructor = separateLines
                [ indentLine classLevel "constructor() {"
                , indentLine methodLevel "super();"
                , separateLines (indentMany methodLevel methods)
                , indentLine classLevel "}"                     ]

        return $ separateLines
            [ indentLine level header <> " {"
            , constructor
            , indentLine level "}"           ]

    -- Try/Catch:
    ----------------------------------------------
    transpileJS level   (TryCatch watchBlock customKey pounceBlock) = do
        let errorKey = maybe "error" getKey customKey
        let blockLevel = succ level

        watch   <- transpileJS level watchBlock
        pounce  <- transpileJS level pounceBlock

        let patch = mconcat [ errorKey, " = ", syncCall Mewlix.pounceError [errorKey], ";" ]

        return $ mconcat
            [ indentLine level "try "
            , watch
            , "\n"
            , indentLine level "catch (", errorKey, ") {\n"
            , indentLine blockLevel patch
            -- A dirty little hack, but it works nicely.
            , Text.tail pounce ]

    -- Assert:
    ----------------------------------------------
    transpileJS level   (Assert expr pos) = do
        value       <- toJS expr
        let call = syncCall Mewlix.assert [ value, errorInfo pos ] <> ";"
        return (indentLine level call)

{- Function -}
-----------------------------------------------------------------
instance ToJavascript MewlixFunction where
    transpileJS level func = do
        let name = (getKey . funcName) func
        params  <- toJS (funcParams func)
        body    <- transpileJS level (funcBody func)
        return $ mconcat [ "(async function ", name, params, " ", body, ").bind(this)" ]

{- Block -}
-----------------------------------------------------------------
instance ToJavascript Block where
    transpileJS level block = do
        let blockLevel = succ level
        transpiled <- mapM (transpileJS blockLevel) (getBlock block)
        return $ separateLines
            [ "{"
            , separateLines transpiled
            , indentLine level "}"              ]


{- Yarn Ball -}
-----------------------------------------------------------------
instance ToJavascript YarnBall where
    transpileJS _ yarnball = do
        let key = maybe mempty joinKey (yarnballKey yarnball)
        let strictPragma = "'use strict';"

        let metadataComment = separateLines
                [ "/*     Auto-generated by the Mewlix compiler:"
                , " * > https://www.github.com/KBMackenzie/mewlix < */" ]

        keyString <- toJS (MewlixString key)

        moduleFunction <- do
            let moduleLevel = toIndent 1
            let block = yarnballBlock yarnball
            transpiled <- mapM (transpileJS moduleLevel) (getBlock block)

            -- The standard library import.
            let stdLibrary = indentLine moduleLevel "const std = Mewlix.Base;\n"

            -- The module bindings to be exported:
            moduleBindings <- do
                let makeTuple :: Key -> Transpiler Text
                    makeTuple (Key binding) = do
                        bindingStr <- toJS (MewlixString binding)
                        return $ mconcat [ "[", bindingStr, ", ", lambda binding, "]" ]

                bindings <- mapM makeTuple (findBindings block)
                let array = (brackets . sepComma) bindings

                let returnStatement = "return " <> instantiate Mewlix.yarnBall [keyString, array] <> ";"
                return $ indentLine moduleLevel returnStatement

            return $ separateLines
                [ "const yarnball = async function yarnball() {"
                , stdLibrary
                , separateLines transpiled
                , moduleBindings
                , "}"                           ]
        
        let footer = syncCall Mewlix.addModule [keyString, "yarnball"]

        -- Separate with two line breaks instead of one.
        let separate :: [Text] -> Text
            separate = Text.intercalate "\n\n"

        return $ separate
                [ metadataComment
                , strictPragma
                , moduleFunction
                , footer        ]
