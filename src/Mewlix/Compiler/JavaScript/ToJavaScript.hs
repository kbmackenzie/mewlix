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
import Mewlix.Compiler.Transpiler (Transpiler, asks, TranspilerContext(..))
import Mewlix.Abstract.Module (ModuleData(..), joinKey, defaultName)
import Mewlix.Abstract.Key (Key(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.String.Escape (escapeString)
import Mewlix.String.Utils (quotes, parens, braces, brackets, sepComma)
import Mewlix.Utils.Show (showT)
import Mewlix.Compiler.Indentation
    ( Indentation
    , zeroIndent
    , toIndent
    , indentLine
    , indentMany
    )
import Mewlix.Compiler.Whitespace (joinLines)
import Mewlix.Compiler.JavaScript.Utils.Expression
    ( instantiate
    , wrap
    , lambda
    , call
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
import qualified Data.HashSet as HashSet
import Control.Monad ((<=<))
import Data.Traversable (for)

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
    transpileJS _ MewlixOutside     = return "this.parent"

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
        return $ call (Mewlix.shelf "create") [ array ]

    transpileJS _ (BoxExpression pairs) = do
        let makeKey :: Key -> Transpiler Text
            makeKey = toJS . MewlixString . getKey

        let makeTuple :: (Key, Expression) -> Transpiler Text
            makeTuple (key, expr) = do
                bind  <- makeKey key
                value <- toJS expr
                return . mconcat $ [ bind, ": ", value ]

        items <- mapM makeTuple pairs
        let array = (braces . sepComma) items
        return $ instantiate (Mewlix.box "create") [ array ]

    -- Boolean operations:
    ----------------------------------------------
    transpileJS _ (BooleanAnd left right) = do
        a  <- toJS left
        fb <- lambda <$> toJS right
        return $ call (Mewlix.boolean "and") [ a, fb ]

    transpileJS _ (BooleanOr left right) = do
        a  <- toJS left
        fb <- lambda <$> toJS right
        return $ call (Mewlix.boolean "or") [ a, fb ]

    -- Ternary operator:
    ----------------------------------------------
    transpileJS _ (TernaryOperation conditionExpr left right) = do
        condition <- toJS conditionExpr
        fa <- lambda <$> toJS left
        fb <- lambda <$> toJS right
        return $ call (Mewlix.boolean "ternary") [ condition, fa, fb ]

    -- Lambda function:
    ----------------------------------------------
    transpileJS _ (LambdaExpression paramExprs bodyExpr) = do
        body   <- toJS bodyExpr
        params <- toJS paramExprs
        wrap (params <> " => " <> body)

    -- Function calls:
    ----------------------------------------------
    transpileJS _ (FunctionCall expr argExprs) = do
        args <- toJS argExprs
        func <- toJS expr
        return (func <> args)

    -- Dot expression:
    ----------------------------------------------
    transpileJS _ (DotExpression objectExpr propertyExpr) = do
        object   <- toJS objectExpr
        property <- toJS propertyExpr
        return (object <> ".box()." <> property)

    -- Lookup expression:
    ----------------------------------------------
    transpileJS _ (LookupExpression objectExpr propertyExpr) = do
        let stringify = call Mewlix.purrify . List.singleton
        object   <- toJS objectExpr
        property <- stringify <$> toJS propertyExpr
        return (object <> ".box()" <> brackets property)

    -- Clowder expressions:
    ----------------------------------------------
    transpileJS _ (ClowderCreate clowderExpr argExprs) = do
        clowder <- toJS clowderExpr
        args    <- toJS argExprs
        let create = call (Mewlix.clowder "instantiate") . List.singleton
        return $ create clowder <> args

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
        return $ call (Mewlix.reflection "typeOf") [arg]

    -- 'Is' / Instance of:
    ----------------------------------------------
    transpileJS _ (IsInstance a b) = do
        args <- mapM toJS [a, b]
        return $ call (Mewlix.reflection "instanceOf") args

    -- 'Claw at'/ Box entries:
    ----------------------------------------------
    transpileJS _ (ClawEntries operand) = do
        arg <- toJS operand
        return $ call (Mewlix.box "pairs") [arg]

    -- IO:
    ----------------------------------------------
    transpileJS _ (MeowExpression expr) = do
        let stringify = call Mewlix.purrify . List.singleton
        arg <- stringify <$> toJS expr
        return $ call (Mewlix.mewlix "meow") [arg]

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
        indentLine level . terminate =<< toJS expr

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
        orIfs  <- map ("else " <>) <$> mapM transpileConditional (NonEmpty.tail conditionals)

        elseBlock <- case else_ of
            Nothing      -> return Text.empty
            (Just block) -> do
                body <- transpileJS level block
                return ("else " <> body)

        joinLines . foldr ($) [] $
            [ (:) (indentLine level lookIf)
            , if null orIfs then id else (:) ((joinLines . indentMany level) orIfs)
            , if Text.null elseBlock then id else (:) (indentLine level elseBlock)  ]

    -- While loop:
    ----------------------------------------------
    transpileJS level   (WhileLoop expr block) = do
        condition   <- asBoolean <$> toJS expr
        body        <- transpileJS level block
        let header = mconcat [ "while (", condition, ") " ]
        indentLine level (header <> body)

    -- Foreach loop:
    ----------------------------------------------
    transpileJS level   (ForEachLoop expr key block) = do
        iterable    <- toJS expr
        body        <- transpileJS level block

        let chase  = call (Mewlix.internal "chase") [iterable]
        let header = mconcat [ "for (const ", getKey key, " of ", chase, ") "]
        indentLine level (header <> body)

    -- Bindings:
    ----------------------------------------------
    transpileJS level    (Variable key expr) = do
        value <- toJS expr
        let declaration = mconcat [ "let ", getKey key, " = ", value, ";" ]
        indentLine level declaration

    transpileJS level    (Constant key expr) = do
        value <- toJS expr
        let declaration = mconcat [ "const ", getKey key, " = ", value, ";" ]
        indentLine level declaration

    -- Functions:
    ----------------------------------------------
    transpileJS level   (FunctionDef func) = do
        funcExpr <- transpileJS level func
        let name = (getKey . funcName) func
        let boundFunc = "(" <> funcExpr <> ").bind(this)"
        let declaration = mconcat [ "const ", name, " = ", boundFunc, ";" ]
        indentLine level declaration

    transpileJS level   (FunctionAssignment expr func) = do
        lvalue   <- toJS expr
        funcExpr <- transpileJS level func
        let boundFunc = "(" <> funcExpr <> ").bind(this)"
        let assignment = mconcat [ lvalue, " = ", boundFunc, ";" ]
        indentLine level assignment

    -- Loop keywords:
    ----------------------------------------------
    transpileJS level   Break = indentLine level "break;"
    transpileJS level   Continue = indentLine level "continue;"

    -- Return keyword:
    ----------------------------------------------
    transpileJS level   (Return expr) = do
        value <- toJS expr
        let ret = mconcat [ "return ", value, ";" ]
        indentLine level ret

    -- Import statement:
    ----------------------------------------------
    transpileJS level   (ImportModule moduleData) = do
        let binding = maybe (defaultName moduleData) getKey (moduleAlias moduleData)
        importValue <- do
            let key = (Key . joinKey . moduleKey) moduleData
            stringKey <- (toJS . MewlixString . getKey) key
            corelib   <- asks library
            if HashSet.member key corelib
                then return $ mconcat [ "mewlix.lib[", stringKey, "]" ]
                else return $ call (Mewlix.modules "get") [stringKey]
        let declaration = mconcat [ "const ", binding, " = ", importValue, ";" ]
        indentLine level declaration

    transpileJS level   (ImportList moduleData keys) = do
        importValue <- do
            let key = (Key . joinKey . moduleKey) moduleData
            stringKey <- (toJS . MewlixString . getKey) key
            corelib   <- asks library
            if HashSet.member key corelib
                then return $ mconcat [ "mewlix.lib[", stringKey, "]" ]
                else return $ call (Mewlix.modules "get") [stringKey]

        let bind :: Key -> Transpiler Text
            bind key = indentLine level $ do
                let value = parens importValue <> ".box()." <> getKey key
                mconcat [ "const ", getKey key, " = ", value, ";" ]

        joinLines (map bind keys)

    -- Assignment statement:
    ----------------------------------------------
    transpileJS level (Assignment key expr) = do
        left  <- toJS key
        right <- toJS expr
        indentLine level . terminate $ (left <> " = " <> right)

    -- Class statement:
    ----------------------------------------------
    transpileJS level   (ClassDef clowder) = do
        let parentValue = maybe MewlixNil (MewlixString . getKey) (classExtends clowder)
        parent <- toJS parentValue
        name   <- (toJS . MewlixString . getKey . className) clowder

        let makeKey :: Key -> Transpiler Text
            makeKey = toJS . MewlixString . getKey

        let makeTuple :: MewlixFunction -> Transpiler Text
            makeTuple func = do
                bind  <- makeKey (funcName func)
                value <- toJS func
                return . mconcat $ [ bind, ": ", value ]

        methods     <- mapM makeTuple (classMethods clowder)
        constructor <- for (classConstructor clowder) $ \func -> do
            let bind = brackets (Mewlix.mewlix "wake")
            value <- toJS func
            return . mconcat $ [ bind, ": ", value ]

        let bindings = maybe methods (: methods) constructor
        let object   = (braces . sepComma) bindings

        let creation = call (Mewlix.clowder "create") [name, parent, object]
        indentLine level . terminate $ creation

    transpileJS level   (SuperCall argExprs) = do
        let superRef = unwrapKeyword Keywords.superRef
        args <- toJS argExprs
        indentLine level . terminate $ (superRef <> args)

    -- Enum statement:
    ----------------------------------------------
    transpileJS level   (EnumDef enum) = do
        let key = (getKey . enumName) enum
        enumBox <- toJS enum
        indentLine level . mconcat $ [ "const ", key, " = ", enumBox, ";" ]

    -- 'Throw' expression:
    ----------------------------------------------
    transpileJS level   (ThrowError expr pos) = do
        let stringify = call Mewlix.purrify . List.singleton
        arg <- stringify <$> toJS expr
        let err = createError CatOnComputer pos arg
        indentLine level . terminate $ ("throw " <> err)

    -- Try/Catch:
    ----------------------------------------------
    transpileJS level   (TryCatch watch customKey pounce) = do
        let errorKey = maybe "error" getKey customKey

        tryBlock   <- transpileJS level watch
        catchBlock <- transpileJS level pounce

        let errorRef = unwrapKeyword Keywords.errorRef
        let defineErrorBox = mconcat
                [ "const ", errorKey, " = ", call (Mewlix.internal "pounce") [errorRef], ";\n" ]

        joinLines
            [ indentLine level ("try " <> tryBlock)
            , indentLine level ("catch (" <> errorRef <> ") {")
            -- A dirty little hack, but it works nicely:
            , indentLine (succ level) (defineErrorBox <> Text.tail catchBlock) ]

    transpileJS level   Rethrow = do
        let errorRef = unwrapKeyword Keywords.errorRef
        indentLine level . terminate $ ("throw " <> errorRef)

    -- Assert:
    ----------------------------------------------
    transpileJS level   (Assert expr pos) = do
        condition <- asBoolean <$> toJS expr
        let failed = call (Mewlix.internal "assertionFail") [ errorInfo pos ]
        indentLine level . terminate . mconcat $
            [ "void (", condition <> " || " <> failed, ")" ]

{- Function -}
-----------------------------------------------------------------
instance ToJavaScript MewlixFunction where
    transpileJS level func = do
        let name = (getKey . funcName) func
        params  <- toJS (funcParams func)
        body    <- transpileJS level (funcBody func)
        return $ mconcat [ "function ", name, params, " ", body ]

{- Function -}
-----------------------------------------------------------------
instance ToJavaScript MewlixEnum where
    transpileJS _ enum = do
        strings <- mapM (toJS . MewlixString . getKey) (enumKeys enum)
        name <- (toJS . MewlixString . getKey . enumName) enum
        let keys = (brackets . sepComma) strings
        return $ call (Mewlix.catTree "create") [name, keys]

{- Block -}
-----------------------------------------------------------------
instance ToJavaScript Block where
    transpileJS level block = do
        let blockLevel = succ level
        let body = map (transpileJS blockLevel) (getBlock block)
        joinLines
            [ return "{"
            , joinLines body
            , indentLine level "}" ]


{- Yarn Ball -}
-----------------------------------------------------------------
instance ToJavaScript YarnBall where
    transpileJS _ yarnball = do
        let key = maybe "main" joinKey (yarnballKey yarnball)
        let topLevel = toIndent 1
        let moduleLevel = toIndent 2

        keyString <- toJS (MewlixString key)
        strictPragma <- indentLine topLevel "'use strict';"

        moduleFunction <- do
            let block = yarnballBlock yarnball

            -- The standard library import.
            noStdBind <- asks noStd
            let stdLibrary = if noStdBind
                then return mempty
                else indentLine moduleLevel "const std = mewlix.lib['std'];"

            -- The module bindings to be exported:
            moduleBindings <- do
                let makeTuple :: Key -> Transpiler Text
                    makeTuple (Key binding) = do
                        bindingStr <- toJS (MewlixString binding)
                        return $ mconcat [ "[", bindingStr, ", ", lambda binding, "]" ]

                bindings <- mapM makeTuple (findBindings block)
                let array = (brackets . sepComma) bindings

                let exports = call (Mewlix.yarnball "bind") [keyString, array]
                let returnStatement = "return " <> exports <> ";"
                return $ indentLine moduleLevel returnStatement

            let header = indentLine topLevel "const yarnball = function yarnball() {"
            let body = map (transpileJS moduleLevel) (getBlock block)

            joinLines
                [ header
                , stdLibrary
                , joinLines body
                , moduleBindings
                , indentLine topLevel "};" ]
        
        let catYarnBall :: [Text] -> Transpiler Text
            catYarnBall text  = do
                prettify <- asks pretty
                let cat = if prettify
                    then Text.intercalate "\n\n"
                    else mconcat
                joinLines . map return $ [ "(function() {" , cat text , "})();" ]

        footer <- indentLine topLevel . terminate $
            call (Mewlix.modules "add") [keyString, "yarnball"]

        catYarnBall
            [ strictPragma
            , moduleFunction
            , footer         ]
