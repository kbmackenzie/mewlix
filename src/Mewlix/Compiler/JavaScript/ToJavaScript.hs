{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.ToJavaScript
( ToJavaScript(..)
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
    , MewlixEnum(..)
    , Conditional(..)
    , YarnBall(..)
    ) 
import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Abstract.Module (ModuleData(..), joinKey, defaultName)
import Mewlix.String.Escape (escapeString)
import Mewlix.String.Utils (quotes, parens, brackets, sepComma, separateLines)
import Mewlix.Compiler.Transpiler (TranspilerContext(..), Transpiler , asks)
import Mewlix.Utils.Show (showT)
import Mewlix.Compiler.Indentation
    ( Indentation
    , zeroIndent
    , toIndent
    , indentLine
    , indentMany
    )
import Mewlix.Compiler.JavaScript.Utils.Expression
    ( instantiate
    , wrap
    , lambda
    , asyncLambda
    , syncCall
    , asyncCall
    , asBoolean
    )
import Mewlix.Compiler.JavaScript.Error (ErrorCode(..), errorInfo, createError)
import Mewlix.Compiler.JavaScript.Utils.Statement (terminate, findBindings)
import Mewlix.Compiler.JavaScript.Operations (binaryOpFunc, unaryOpFunc)
import qualified Mewlix.Compiler.JavaScript.Constants as Mewlix
import qualified Mewlix.Keywords.LanguageKeywords as Keywords
import Mewlix.Keywords.Types (unwrapKeyword)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.HashMap.Strict as HashMap
import Data.Char (isSpace)
import Control.Monad ((<=<))

class ToJavaScript a where
    transpileJS :: Indentation -> a -> Transpiler Text

    toJS :: a -> Transpiler Text
    toJS = transpileJS zeroIndent

{- Primitives -}
-----------------------------------------------------------------
instance ToJavaScript Primitive where
    transpileJS _ (MewlixInt i)     = (return . showT) i
    transpileJS _ (MewlixFloat f)   = (return . showT) f
    transpileJS _ (MewlixString s)  = (return . quotes . escapeString) s
    transpileJS _ (MewlixBool b)    = (return . Text.toLower . showT) b
    transpileJS _ MewlixNil         = return "null"
    transpileJS _ MewlixHome        = return "this"

{- Expression -}
-----------------------------------------------------------------
instance ToJavaScript Expression where
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
        return $ asyncCall (Mewlix.boolean "and") [ a, fb ]

    transpileJS _ (BooleanOr left right) = do
        a  <- toJS left
        fb <- asyncLambda <$> toJS right
        return $ asyncCall (Mewlix.boolean "or") [ a, fb ]

    -- Ternary operator:
    ----------------------------------------------
    transpileJS _ (TernaryOperation conditionExpr left right) = do
        condition <- toJS conditionExpr
        fa <- asyncLambda <$> toJS left
        fb <- asyncLambda <$> toJS right
        return $ asyncCall (Mewlix.boolean "ternary") [ condition, fa, fb ]

    -- Lambda function:
    ----------------------------------------------
    transpileJS _ (LambdaExpression paramExprs bodyExpr) = do
        body   <- toJS bodyExpr
        params <- toJS paramExprs
        wrap ("async " <> params <> " => " <> body)

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
        wrap $ mconcat ["await new ", clowder, "()[", Mewlix.wake, "]", args]

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
    transpileJS _ (AskType operand) = do
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

    -- IO:
    ----------------------------------------------
    transpileJS _ (MeowExpression expr) = do
        let stringify = syncCall Mewlix.purrify . List.singleton
        arg <- stringify <$> toJS expr
        return $ asyncCall Mewlix.meow [arg]

    transpileJS _ (ListenExpression expr) = do
        let stringify = syncCall Mewlix.purrify . List.singleton
        let nil = toJS MewlixNil
        arg <- maybe nil (fmap stringify . toJS) expr 
        return $ asyncCall Mewlix.listen [arg]

{- Params -}
-----------------------------------------------------------------
instance ToJavaScript Params where
    transpileJS _ = wrap . sepComma . map getKey . getParams

{- Arguments -}
-----------------------------------------------------------------
instance ToJavaScript Arguments where
    transpileJS _ = wrap . sepComma <=< mapM toJS . getArguments

{- Statements -}
-----------------------------------------------------------------
instance ToJavaScript Statement where
    -- Note: Every statement should be transpiled with no trailing linebreaks.
    -- Linebreaks between statements are added during Block transpilation.

    -- Expressions:
    ----------------------------------------------
    transpileJS level   (ExpressionStatement expr) = do
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

        lookIf <- transpileConditional (NonEmpty.head conditionals)
        orIfs  <- mapM transpileConditional (NonEmpty.tail conditionals)

        elseBlock <- case else_ of
            Nothing      -> return Text.empty
            (Just block) -> do
                body <- transpileJS level block
                return ("else " <> body)

        return . separateLines . filter (Text.any (not . isSpace)) $
            [ indentLine level lookIf
            , (separateLines . indentMany level . map ("else " <>)) orIfs
            , indentLine level elseBlock                                  ]

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

        let chase  = syncCall Mewlix.canChase [iterable]
        let header = indentLine level $ mconcat
                [ "for (const ", getKey key, " of ", chase, ") "]

        return (header <> body)

    -- Bindings:
    ----------------------------------------------
    transpileJS level    (Variable key expr) = do
        value <- toJS expr
        let declaration = mconcat [ "let ", getKey key, " = ", value, ";" ]
        return (indentLine level declaration)

    transpileJS level    (Constant key expr) = do
        value <- toJS expr
        let declaration = mconcat [ "const ", getKey key, " = ", value, ";" ]
        return (indentLine level declaration)

    -- Functions:
    ----------------------------------------------
    transpileJS level   (FunctionDef func) = do
        funcExpr <- transpileJS level func
        let declaration = mconcat [ "const ", (getKey . funcName) func, " = ", funcExpr, ";" ]
        return (indentLine level declaration)

    transpileJS level   (FunctionAssignment expr func) = do
        lvalue   <- toJS expr
        funcExpr <- transpileJS level func
        let assignment = mconcat [ lvalue, " = ", funcExpr, ";" ]
        return (indentLine level assignment)

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
            bind key = indentLine level $ do
                let value = parens importValue <> ".box()." <> getKey key
                mconcat [ "const ", getKey key, " = ", value, ";" ]

        (return . separateLines) (map bind keys)

    -- Assignment statement:
    ----------------------------------------------
    transpileJS level (Assignment key expr) = do
        left  <- toJS key
        right <- toJS expr
        return . indentLine level . terminate $ (left <> " = " <> right)

    -- Class statement:
    ----------------------------------------------
    transpileJS level   (ClassDef clowder) = do
        let extends = maybe Mewlix.clowder getKey (classExtends clowder)
        let binding = (getKey . className) clowder
        let header = mconcat [ "const ", binding, " = class ", binding, " extends ", extends ]

        let classLevel  = succ level
        let methodLevel = succ classLevel

        let wake = Key (unwrapKeyword Keywords.constructor)

        let superRef = unwrapKeyword Keywords.superRef
        let defineSuper = mconcat
                ["const ", superRef, " = this[", Mewlix.wake, "];"]

        let transpileMethod :: MewlixFunction -> Transpiler Text
            transpileMethod func = do
                funcExpr <- transpileJS methodLevel func
                let bind = if funcName func == wake
                    then "this[" <> Mewlix.wake <> "]"
                    else "this." <> (getKey . funcName) func
                return $ mconcat [ bind, " = ", funcExpr, ";" ]

        methods <- mapM transpileMethod (classMethods clowder)
        let constructor = separateLines
                [ indentLine classLevel "constructor() {"
                , indentLine methodLevel "super();"
                , indentLine methodLevel defineSuper
                , separateLines (indentMany methodLevel methods)
                , indentLine classLevel "}"                     ]

        return $ separateLines
            [ indentLine level header <> " {"
            , constructor
            , indentLine level "}"           ]

    transpileJS level   (SuperCall argExprs) = do
        let superRef = unwrapKeyword Keywords.superRef
        args <- toJS argExprs
        return . indentLine level . terminate $ ("await " <> superRef <> args)

    -- Enum statement:
    ----------------------------------------------
    transpileJS level   (EnumDef enum) = do
        let key = (getKey . enumName) enum
        enumBox <- toJS enum
        return . indentLine level . mconcat $ [ "const ", key, " = ", enumBox, ";" ]

    -- 'Throw' expression:
    ----------------------------------------------
    transpileJS level   (ThrowError expr pos) = do
        let stringify = syncCall Mewlix.purrify . List.singleton
        arg <- stringify <$> toJS expr
        let err = createError CatOnComputer pos arg
        return . indentLine level . terminate $ ("throw " <> err)

    -- Try/Catch:
    ----------------------------------------------
    transpileJS level   (TryCatch watch customKey pounce) = do
        let errorKey = maybe "error" getKey customKey

        tryBlock   <- transpileJS level watch
        catchBlock <- transpileJS level pounce

        let errorRef = unwrapKeyword Keywords.errorRef
        let defineErrorBox = mconcat
                [ "const ", errorKey, " = ", syncCall Mewlix.pounceError [errorRef], ";\n" ]

        return $ mconcat
            [ indentLine level ("try " <> tryBlock <> "\n")
            , indentLine level ("catch (" <> errorRef <> ") {\n")
            , indentLine (succ level) defineErrorBox
            -- A dirty little hack, but it works nicely:
            , Text.tail catchBlock ]

    transpileJS level   Rethrow = do
        let errorRef = unwrapKeyword Keywords.errorRef
        return . indentLine level . terminate $ ("throw " <> errorRef)

    -- Assert:
    ----------------------------------------------
    transpileJS level   (Assert expr pos) = do
        value       <- toJS expr
        let call = syncCall Mewlix.assert [ value, errorInfo pos ] <> ";"
        return (indentLine level call)

{- Function -}
-----------------------------------------------------------------
instance ToJavaScript MewlixFunction where
    transpileJS level func = do
        let name = (getKey . funcName) func
        params  <- toJS (funcParams func)
        body    <- transpileJS level (funcBody func)
        return $ mconcat [ "(async function ", name, params, " ", body, ").bind(this)" ]

{- Function -}
-----------------------------------------------------------------
instance ToJavaScript MewlixEnum where
    transpileJS _ enum = do
        strings <- mapM (toJS . MewlixString . getKey) (enumKeys enum)
        let arg = (brackets . sepComma) strings
        return $ instantiate Mewlix.enum [arg]

{- Block -}
-----------------------------------------------------------------
instance ToJavaScript Block where
    transpileJS level block = do
        let blockLevel = succ level
        transpiled <- mapM (transpileJS blockLevel) (getBlock block)
        return $ separateLines
            [ "{"
            , separateLines transpiled
            , indentLine level "}"              ]


{- Yarn Ball -}
-----------------------------------------------------------------
instance ToJavaScript YarnBall where
    transpileJS _ yarnball = do
        let key = maybe Mewlix.defaultKey joinKey (yarnballKey yarnball)
        let strictPragma = "'use strict';"

        let metadataComment = separateLines
                [ "/*     Auto-generated by the Mewlix compiler"
                , " * > https://www.github.com/kbmackenzie/mewlix < */" ]

        keyString <- toJS (MewlixString key)

        moduleFunction <- do
            let moduleLevel = toIndent 1
            let block = yarnballBlock yarnball
            transpiled <- mapM (transpileJS moduleLevel) (getBlock block)

            -- The standard library import.

            noStd <- asks transpilerNoStd
            let stdLibrary = if noStd
                then mempty
                else indentLine moduleLevel "const std = Mewlix.Base;\n"

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
        -- Always end with a newline character.
        let separate :: [Text] -> Text
            separate = (`Text.snoc` '\n') . Text.intercalate "\n\n"

        return $ separate
                [ metadataComment
                , strictPragma
                , moduleFunction
                , footer         ]
