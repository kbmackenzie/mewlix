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
import Mewlix.Compiler.Indentation
    ( Indentation
    , zeroIndent
    , toIndent
    , indentLine
    , indentMany
    )
import Mewlix.Compiler.Analysis (Operation(..), analyze)
import Mewlix.Compiler.JavaScript.Declare (operation, declareOperations)
import Mewlix.Compiler.Whitespace (joinLines)
import Mewlix.Compiler.JavaScript.Utils
    ( stringify
    , boolean
    , terminate
    , findBindings
    , parensAround
    , lambda
    , call
    )
import Mewlix.Compiler.JavaScript.Error (ErrorCode(..), errorInfo, createError)
import Mewlix.Compiler.JavaScript.Operations (binaryOpFunc, unaryOpFunc)
import qualified Mewlix.Compiler.JavaScript.Constants as Mewlix
import Mewlix.String.Escape (escapeString)
import Mewlix.String.Utils (quotes, parens, brackets, sepComma)
import Mewlix.Utils.Show (showT)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.HashSet as HashSet
import Control.Monad ((<=<))
import Data.Traversable (for)
import Data.Maybe (fromMaybe)

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
    transpileJS _ MewlixOutside     = return "this.outside()"

{- Expression -}
-----------------------------------------------------------------
instance ToJavaScript Expression where
    transpileJS _ (PrimitiveExpr prim) = toJS prim

    -- Names:
    ----------------------------------------------
    transpileJS _ (Identifier key) = (return . getKey) key
    transpileJS _ (ObjectProperty key) = (toJS . MewlixString . getKey) key

    -- Lists + boxes:
    ----------------------------------------------
    transpileJS level (ShelfExpression exprs) = do
        let makeItem :: Expression -> Transpiler Text
            makeItem expr = do
                value <- transpileJS ((succ . succ) level) expr
                indentLine (succ level) (value <> ",")

        let items = map makeItem exprs
        array <- joinLines
            [ return "["
            , joinLines items
            , indentLine level "]" ]
        return $ call (Mewlix.shelf "create") [ array ]

    transpileJS level (BoxExpression pairs) = do
        let makeKey :: Key -> Transpiler Text
            makeKey = toJS . MewlixString . getKey

        let makeTuple :: (Key, Expression) -> Transpiler Text
            makeTuple (key, expr) = do
                bind  <- makeKey key
                value <- transpileJS (succ level) expr
                indentLine (succ level) . mconcat $ [ bind, ": ", value, "," ]

        let items = map makeTuple pairs
        table <- joinLines
            [ return "{"
            , joinLines items
            , indentLine level "}" ]
        return $ call (Mewlix.box "create") [ table ]

    -- Boolean operations:
    ----------------------------------------------
    transpileJS level (BooleanAnd left right) = do
        let ref = operation And
        a <- transpileJS level left
        b <- transpileJS level right
        let assignment = parens . mconcat $ [ ref, "=", a ]
        parensAround . mconcat $
            [ boolean assignment, "?", b, ":", ref ]

    transpileJS level (BooleanOr left right) = do
        let ref = operation Or
        a <- transpileJS level left
        b <- transpileJS level right
        let assignment = parens . mconcat $ [ ref, "=", a ]
        parensAround . mconcat $
            [ boolean assignment, "?", ref, ":", b ]

    -- String operations:
    ----------------------------------------------
    transpileJS level (StringCoerce expr) = do
        stringify <$> transpileJS level expr

    -- Ternary operator:
    ----------------------------------------------
    transpileJS level (TernaryOperation condition left right) = do
        a <- transpileJS level condition
        b <- transpileJS level left
        c <- transpileJS level right
        parensAround . mconcat $ [ boolean a, "?", b, ":", c ]

    -- Lambda function:
    ----------------------------------------------
    transpileJS level (LambdaExpression lambdaParams lambdaBody) = do
        let operations = analyze lambdaBody

        let addDeclarations :: [Transpiler Text] -> [Transpiler Text]
            addDeclarations = maybe id ((:) . indent) . declareOperations $ operations
                where indent = indentLine (succ level)

        let returns = transpileJS (succ level) (Return lambdaBody)
        let body    = joinLines . addDeclarations . List.singleton $ returns

        let params  = transpileJS level lambdaParams
        let header  = params >>= \xs -> return ("function" <> xs <> " {")

        let bindFunction :: Text -> Transpiler Text
            bindFunction func = return $ "(" <> func <> ").bind(this)"

        bindFunction =<< joinLines
            [ header
            , body
            , indentLine level "}" ]

    -- Function calls:
    ----------------------------------------------
    transpileJS level (FunctionCall funcExpr argExprs) = do
        func <- transpileJS level funcExpr
        args <- transpileJS level argExprs
        return (func <> args)

    -- Dot expression:
    ----------------------------------------------
    transpileJS level (DotExpression objectExpr propertyExpr) = do
        object   <- transpileJS level objectExpr
        property <- transpileJS level propertyExpr
        return $ call (object <> ".get") [property]

    -- Lookup expression:
    ----------------------------------------------
    transpileJS level (LookupExpression objectExpr propertyExpr) = do
        object   <- transpileJS level objectExpr
        property <- transpileJS level propertyExpr
        return $ call (object <> ".get") [property]

    -- Clowder expressions:
    ----------------------------------------------
    transpileJS level (ClowderCreate clowderExpr argExprs) = do
        clowder <- transpileJS level clowderExpr
        args    <- transpileJS level argExprs
        let create = call (Mewlix.clowder "instantiate") . List.singleton
        return $ create clowder <> args

    -- Binary operations:
    ----------------------------------------------
    transpileJS level (BinaryOperation op left right) = do
        let func = binaryOpFunc op
        args <- mapM (transpileJS level) [left, right]
        return $ func args

    -- Unary operations:
    ----------------------------------------------
    transpileJS level (UnaryOperation op operand) = do
        let func = unaryOpFunc op
        arg  <- transpileJS level operand
        return $ func [arg]

    -- Type of:
    ----------------------------------------------
    transpileJS level (AskType operand) = do
        arg <- transpileJS level operand
        return $ call (Mewlix.reflection "typeOf") [arg]

    -- 'is' / Instance of:
    ----------------------------------------------
    transpileJS level (IsInstance a b) = do
        args <- mapM (transpileJS level) [a, b]
        return $ call (Mewlix.reflection "instanceOf") args

    -- 'claw at' / Box entries:
    ----------------------------------------------
    transpileJS level (ClawEntries operand) = do
        arg <- transpileJS level operand
        return $ call (Mewlix.box "pairs") [arg]

    -- I/O:
    ----------------------------------------------
    transpileJS level (MeowExpression expr) = do
        arg <- transpileJS level expr
        return $ call (Mewlix.mewlix "meow") [arg]

{- Params -}
-----------------------------------------------------------------
instance ToJavaScript Params where
    transpileJS _ = parensAround . sepComma . map getKey . getParams

{- Arguments -}
-----------------------------------------------------------------
instance ToJavaScript Arguments where
    transpileJS level = parensAround . sepComma <=< mapM (transpileJS level) . getArguments

{- Statements -}
-----------------------------------------------------------------
instance ToJavaScript Statement where
    -- Note: Every statement should be transpiled with no trailing linebreaks.
    -- Linebreaks between statements are added during Block transpilation.

    -- Expressions:
    ----------------------------------------------
    transpileJS level   (ExpressionStatement expr) = do
        indentLine level . terminate =<< transpileJS level expr

    -- Bindings:
    ----------------------------------------------
    transpileJS level    (Variable key expr) = do
        value <- transpileJS level expr
        let declaration = mconcat [ "let ", getKey key, " = ", value, ";" ]
        indentLine level declaration

    transpileJS level    (Constant key expr) = do
        value <- transpileJS level expr
        let declaration = mconcat [ "const ", getKey key, " = ", value, ";" ]
        indentLine level declaration

    -- Functions:
    ----------------------------------------------
    transpileJS level   (FunctionDef func) = do
        funcExpr <- transpileJS level func
        let name = (getKey . functionName) func
        let boundFunc = "(" <> funcExpr <> ").bind(this)"
        let declaration = mconcat [ "const ", name, " = ", boundFunc, ";" ]
        indentLine level declaration

    transpileJS level   (FunctionAssignment expr func) = do
        let assignment :: Transpiler (Text -> Text)
            assignment = do
                left <- transpileJS level expr
                return $ \right -> mconcat [ left, " = ", right ]

        let createSetter :: Expression -> Expression -> Transpiler (Text -> Text)
            createSetter a b = do
                object   <- transpileJS level a
                property <- transpileJS level b
                return $ \value -> call (object <> ".set") [property, value]

        funcExpr <- transpileJS level func
        let boundFunc = "(" <> funcExpr <> ").bind(this)"

        assigner <- case expr of
            (LookupExpression a b) -> createSetter a b
            (DotExpression a b)    -> createSetter a b
            _                      -> assignment
        indentLine level . terminate $ assigner boundFunc

    -- Assignment statement:
    ----------------------------------------------
    transpileJS level (Assignment lvalue rvalue) = do
        let assignment :: Transpiler Text
            assignment = do
                left  <- transpileJS level lvalue
                right <- transpileJS level rvalue
                return . mconcat $ [ left, " = ", right ]

        let createSetter :: Expression -> Expression -> Transpiler Text
            createSetter obj prop = do
                object   <- transpileJS level obj
                property <- transpileJS level prop
                value    <- transpileJS level rvalue
                return $ call (object <> ".set") [property, value]

        indentLine level . terminate =<< case lvalue of
            (LookupExpression obj prop) -> createSetter obj prop
            (DotExpression    obj prop) -> createSetter obj prop
            _                           -> assignment
                
    -- Control flow:
    ----------------------------------------------
    transpileJS level   (IfElse conditionals else_) = do
        let transpileConditional :: Conditional -> Transpiler Text
            transpileConditional (Conditional expr block) = do
                condition <- boolean <$> transpileJS level expr
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
        condition   <- boolean <$> transpileJS level expr
        body        <- transpileJS level block
        let header = mconcat [ "while (", condition, ") " ]
        indentLine level (header <> body)

    -- Foreach loop:
    ----------------------------------------------
    transpileJS level   (ForEachLoop expr key block) = do
        iterable    <- transpileJS level expr
        body        <- transpileJS level block

        let chase  = call (Mewlix.internal "chase") [iterable]
        let header = mconcat [ "for (const ", getKey key, " of ", chase, ") "]
        indentLine level (header <> body)

    -- Loop keywords:
    ----------------------------------------------
    transpileJS level   Break = indentLine level "break;"
    transpileJS level   Continue = indentLine level "continue;"

    -- Return keyword:
    ----------------------------------------------
    transpileJS level   (Return expr) = do
        value <- transpileJS level expr
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
            bind key = indentLine level =<< do
                stringKey <- (toJS . MewlixString . getKey) key
                let value = call (importValue <> ".get") [stringKey]
                return . mconcat $ [ "const ", getKey key, " = ", value, ";" ]

        joinLines (map bind keys)

    -- Class statement:
    ----------------------------------------------
    transpileJS level   (ClassDef clowder) = do
        let nil = PrimitiveExpr MewlixNil
        let parentValue = maybe nil Identifier (classExtends clowder)
        parent <- transpileJS level parentValue
        name   <- (toJS . MewlixString . getKey . className) clowder

        let makeKey :: Key -> Transpiler Text
            makeKey = toJS . MewlixString . getKey

        let makeTuple :: MewlixFunction -> Transpiler Text
            makeTuple func = do
                bind  <- makeKey (functionName func)
                value <- transpileJS (succ level) func
                indentLine (succ level) . mconcat $ [ bind, ": ", value, "," ]

        let methods = map makeTuple (classMethods clowder)
        constructor <- for (classConstructor clowder) $ \func -> do
            let bind = brackets (Mewlix.mewlix "wake")
            value <- transpileJS (succ level) func
            indentLine (succ level) . mconcat $ [ bind, ": ", value, "," ]

        let bindings = maybe methods ((: methods) . return) constructor
        table <- joinLines
            [ return "{"
            , joinLines bindings
            , indentLine level "}" ]

        let creation = call (Mewlix.clowder "create") [name, parent, table]
        indentLine level . mconcat $
            [ "const ", (getKey . className) clowder, " = ", creation, ";" ]

    transpileJS level   (SuperCall argExprs) = do
        let super = "this.outside().wake"
        args <- transpileJS level argExprs
        indentLine level . terminate $ (super <> args)

    -- Enum statement:
    ----------------------------------------------
    transpileJS level   (EnumDef enum) = do
        let key = (getKey . enumName) enum
        enumBox <- transpileJS level enum
        indentLine level . mconcat $ [ "const ", key, " = ", enumBox, ";" ]

    -- 'Throw' statement:
    ----------------------------------------------
    transpileJS level   (ThrowError expr pos) = do
        arg <- transpileJS level expr
        let err = createError CatOnComputer pos arg
        indentLine level . terminate $ ("throw " <> err)

    -- Try/Catch:
    ----------------------------------------------
    transpileJS level   (TryCatch watch customKey pounce) = do
        let errorKey = maybe "error" getKey customKey

        tryBlock   <- transpileJS level watch
        catchBlock <- transpileJS level pounce

        let defineErrorBox = mconcat
                [ "const ", errorKey, " = ", call (Mewlix.internal "pounce") [Mewlix.errorRef], ";\n" ]

        joinLines
            [ indentLine level ("try " <> tryBlock)
            , indentLine level ("catch (" <> Mewlix.errorRef <> ") {")
            -- A cutesy little hack, but it works nicely. :'>
            , indentLine (succ level) (defineErrorBox <> Text.tail catchBlock) ]

    transpileJS level   Rethrow = do
        indentLine level . terminate $ ("throw " <> Mewlix.errorRef)

    -- Assert:
    ----------------------------------------------
    transpileJS level   (Assert expr pos) = do
        let assertion :: Transpiler Text
            assertion = do
                condition <- boolean <$> transpileJS level expr
                let failed = call (Mewlix.internal "assertionFail") [ errorInfo pos ]
                indentLine level . terminate . mconcat $
                    [ "void (", condition <> " || " <> failed, ")" ]

        releaseMode <- asks release
        if releaseMode then return mempty else assertion

{- Function -}
-----------------------------------------------------------------
instance ToJavaScript MewlixFunction where
    transpileJS level func = do
        let operations = analyze (functionBody func)

        let transpileBody :: MewlixFunction -> [Transpiler Text]
            transpileBody = map transpile . getBlock . functionBody
                where transpile = transpileJS (succ level)

        let addDeclarations :: [Transpiler Text] -> [Transpiler Text]
            addDeclarations = maybe id ((:) . indent) . declareOperations $ operations
                where indent = indentLine (succ level)

        params <- transpileJS level (functionParams func)
        let name   = (getKey . functionName) func
        let body   = addDeclarations . transpileBody $ func
        let header = mconcat ["function ", name, params, " {"]
        joinLines
            [ return header
            , joinLines body
            , indentLine level "}" ]

{- Enums -}
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

            -- Declarations of operation-specific shadow variables.
            let declarations :: Transpiler Text
                declarations = do
                    let operations = analyze block
                    let line       = fromMaybe mempty . declareOperations $ operations
                    indentLine moduleLevel line

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
                , declarations
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
