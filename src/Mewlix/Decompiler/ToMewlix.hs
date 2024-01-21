{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Decompiler.ToMewlix
( ToMewlix(..)
, IndentLevel
) where

import Mewlix.Abstract.AST
import Mewlix.String.Utils (surround)
import Mewlix.String.Escape (escapeString)
import Mewlix.Keywords.Types (unwrapKeyword, unwrapSymbol, joinWords)
import qualified Mewlix.Keywords.Constants as Keywords
import Mewlix.Utils.Show (showT)
import Data.Text (Text)
import qualified Data.Text as Text

{- Typeclass -}
----------------------------------------------------
type IndentLevel = Int

class ToMewlix a where
    toMewlixStr :: IndentLevel -> a -> Text

    toMewlix :: a -> Text
    toMewlix = toMewlixStr 0

{- A class for types that can be turned into Mewlix code.
 - This can be used for decompilation.
 -
 - The output is guaranteed to be valid Mewlix if the input is valid Mewlix.
 - These functions do not perform any form of semantic checking, however.
 -
 - The 'toMewlixStr' function is the minimal definition for this typeclass.
 - The 'toMewlix' function should always be preferred over 'toMewlixStr' (outside of instance declarations). -}

{- Utils -}
----------------------------------------------------
spaceSep :: [Text] -> Text
spaceSep = Text.intercalate " "

lineSep :: [Text] -> Text
lineSep = Text.intercalate "\n"

indent :: IndentLevel -> Text -> Text
indent level = do
    let indentation = Text.pack (replicate level ' ')
    Text.append indentation

flatten :: [Text] -> Text
flatten = Text.intercalate ", "

flattenLn :: [Text] -> Text
flattenLn = Text.intercalate ",\n"

between :: Char -> Char -> Text -> Text
between open close text = Text.concat
    [ Text.singleton open
    , text
    , Text.singleton close ]

parens :: Text -> Text
parens = between '(' ')'

brackets :: Text -> Text
brackets = between '[' ']'

{- Basic Instances -}
------------------------------------------------------
instance ToMewlix Text where
    toMewlixStr _ = surround '"' . escapeString 

instance ToMewlix Int where
    toMewlixStr _ = showT

instance ToMewlix Double where
    toMewlixStr _ = showT

instance ToMewlix Bool where
    toMewlixStr _ = Text.toLower . showT

{- Mewlix AST -}
----------------------------------------------------
instance ToMewlix Primitive where
    toMewlixStr _ (MewlixInt n)    = toMewlix n
    toMewlixStr _ (MewlixString s) = toMewlix s
    toMewlixStr _ (MewlixFloat f)  = toMewlix f
    toMewlixStr _ (MewlixBool b)   = toMewlix b
    toMewlixStr _ MewlixNil        = unwrapKeyword Keywords.nil
    toMewlixStr _ MewlixHome       = unwrapKeyword Keywords.home
    toMewlixStr _ MewlixSuper      = unwrapKeyword Keywords.super

instance ToMewlix Params where
    toMewlixStr level (Params params) = do
        (parens . flatten . map (toMewlixStr level)) params

instance ToMewlix Block where
    toMewlixStr level (Block block) = do
        let newLevel = level + 1
        (lineSep . map (toMewlixStr newLevel)) block

instance ToMewlix Expression where
    toMewlixStr level (PrimitiveExpr prim) = toMewlixStr level prim

    toMewlixStr _     (Identifier a) = a
    toMewlixStr _     (ObjectProperty a) = a

    toMewlixStr level (BooleanAnd a b) = spaceSep
        [ toMewlixStr level a
        , unwrapKeyword Keywords.and
        , toMewlixStr level b ]

    toMewlixStr level (BooleanOr a b) = spaceSep
        [ toMewlixStr level a
        , unwrapKeyword Keywords.or
        , toMewlixStr level b ]

    toMewlixStr level (BinaryOperation op a b) = spaceSep
        [ toMewlixStr level a
        , toMewlixStr level op
        , toMewlixStr level b ]

    toMewlixStr level (UnaryOperation op a) = spaceSep
        [ toMewlixStr level op
        , toMewlixStr level a ]

    toMewlixStr level (TernaryOperation condition a b) = spaceSep
        [ toMewlixStr level condition
        , "?"
        , toMewlixStr level a
        , ":"
        , toMewlixStr level b ]

    toMewlixStr level (ListExpression items) = do
        let newLevel = level + 1
        let prettify :: (ToMewlix a) => [a] -> Text
            prettify = flattenLn . map (indent newLevel . toMewlixStr newLevel)
        lineSep [ "[", prettify items, indent level "]" ]

    toMewlixStr level (BoxExpression pairs) = do
        let newLevel = level + 1

        let prettyPair :: (Key, Expression) -> Text
            prettyPair (key, expr) = Text.concat
                [ key
                , ": "
                , toMewlixStr newLevel expr ]

        let prettify :: [(Key, Expression)] -> Text
            prettify = flattenLn . map (indent newLevel . prettyPair)

        let header = unwrapSymbol Keywords.box `Text.append` " ["
        lineSep [ header, prettify pairs, indent level "]" ]

    toMewlixStr level (Assignment a b) = spaceSep
        [ toMewlixStr level a
        , "="
        , toMewlixStr level b ]

    toMewlixStr level (Increment a) = spaceSep
        [ joinWords Keywords.paw
        , toMewlixStr level a ]

    toMewlixStr level (Decrement a) = spaceSep
        [ joinWords Keywords.claw
        , toMewlixStr level a ]

    toMewlixStr level (ListPush a b) = spaceSep
        [ toMewlixStr level a
        , unwrapKeyword Keywords.push
        , toMewlixStr level b ]

    toMewlixStr level (ListPop a) = spaceSep
        [ joinWords Keywords.pop
        , toMewlixStr level a ]

    toMewlixStr level (LambdaExpression params body) = spaceSep
        [ unwrapSymbol Keywords.lambda
        , toMewlixStr level params
        , "=>"
        , toMewlixStr level body ]

    toMewlixStr level (FunctionCall params func) = Text.concat
        [ toMewlixStr level func
        , (parens . flatten . map (toMewlixStr level)) params ]

    toMewlixStr level (DotExpression a b) = Text.concat
        [ toMewlixStr level a
        , "."
        , toMewlixStr level b ]

    toMewlixStr level (LookupExpression a b) = Text.concat
        [ toMewlixStr level a
        , brackets (toMewlixStr level b) ]

instance ToMewlix BinaryOp where
    toMewlixStr _ op = case op of
        Addition        -> "+"
        Subtraction     -> "-"
        Multiplication  -> "*"
        Division        -> "\\"
        Modulo          -> "%"
        Power           -> "^"
        ListConcat      -> ".."
        Equal           -> "=="
        LessThan        -> "<"
        GreaterThan     -> ">"
        NotEqual        -> "!="
        GreaterOrEqual  -> ">="
        LesserOrEqual   -> "<="
        
instance ToMewlix UnaryOp where
    toMewlixStr _ op = case op of
        Negation        -> "-"
        ListPeek        -> unwrapKeyword Keywords.peek
        BooleanNot      -> unwrapKeyword Keywords.not
        LengthLookup    -> "?!"

instance ToMewlix MewlixFunction where
    toMewlixStr level func = do
        let header = spaceSep
                [ unwrapSymbol Keywords.function
                , funcName func
                , toMewlixStr level (funcParams func) ]
        lineSep
            [ indent level header
            , toMewlixStr level (funcBody func)
            , indent level (unwrapKeyword Keywords.end) ]

instance ToMewlix LiftedExpression where
    toMewlixStr level (LiftExpression expr) = toMewlixStr level expr
    toMewlixStr level (LiftDeclaration key expr) = spaceSep
        [ unwrapKeyword Keywords.local
        , key
        , "="
        , toMewlixStr level expr ]

instance ToMewlix MewlixClass where
    toMewlixStr level clowder = do
        let newLevel = level + 1
        let parent = case classExtends clowder of
                Nothing     -> Text.empty
                (Just key)  -> spaceSep [ (unwrapKeyword . snd) Keywords.clowder, key ]
        let header = spaceSep
                [ (unwrapKeyword . fst) Keywords.clowder
                , className clowder
                , parent ]
        let methods = Text.intercalate "\n\n" . map (toMewlixStr newLevel) . classMethods
        lineSep
            [ indent level header
            , methods clowder
            , indent level (unwrapKeyword Keywords.end) ]

instance ToMewlix Statement where
    toMewlixStr level   (ExpressionStatement expr) = indent level (toMewlixStr level expr)

    toMewlixStr level   (WhileLoop condition block) = do
        let header = Text.concat
                [ unwrapKeyword Keywords.while
                , (parens . toMewlixStr level) condition ]
        lineSep
            [ indent level header
            , toMewlixStr level block
            , indent level (unwrapKeyword Keywords.end) ]

    toMewlixStr level   (ForLoop (a, b, c) block) = do
        let (start, middle, end) = Keywords.takeDo
        let header = spaceSep
                [ unwrapKeyword start
                , parens (toMewlixStr level a)
                , joinWords middle
                , parens (toMewlixStr level b)
                , unwrapKeyword end
                , parens (toMewlixStr level c) ]
        lineSep
            [ indent level header
            , toMewlixStr level block
            , indent level (unwrapKeyword Keywords.end) ]

    toMewlixStr level   (IfElse condition ifb elseb) = do
        let header = spaceSep
                [ unwrapKeyword Keywords.if_
                , parens (toMewlixStr level condition) ]
        lineSep
            [ indent level header
            , toMewlixStr level ifb
            , indent level (unwrapKeyword Keywords.else_)
            , toMewlixStr level elseb
            , unwrapKeyword Keywords.end ]

    toMewlixStr level   (FunctionDef func) = toMewlixStr level func

    toMewlixStr level   (Declaration key expr) = do
        let statement = spaceSep
                [ unwrapKeyword Keywords.local
                , key
                , "="
                , toMewlixStr level expr ]
        indent level statement

    toMewlixStr level   (ClassDef clowder) = toMewlixStr level clowder

    toMewlixStr level   (ImportStatement path key) = do
        let (start, end) = Keywords.takes
        let takes = spaceSep [ unwrapKeyword start, toMewlix path ]
        let qualified = case key of
                Nothing  -> Text.empty
                (Just x) -> spaceSep [ unwrapKeyword end, x ]
        (indent level . spaceSep) [ takes, qualified ]

    toMewlixStr level   (Return expr) = do
        let statement = spaceSep
                [ unwrapKeyword Keywords.ret
                , toMewlixStr level expr ]
        indent level statement

    toMewlixStr level   (TryCatch tryb (condition, catchb)) = do
        let catchCondition = case condition of
                Nothing     -> Text.empty
                (Just expr) -> toMewlixStr level expr
        let catchHeader = spaceSep [ unwrapKeyword Keywords.catch, catchCondition ]
        lineSep
            [ indent level (unwrapKeyword Keywords.try)
            , toMewlixStr level tryb
            , indent level catchHeader
            , toMewlixStr level catchb
            , indent level (unwrapKeyword Keywords.try) ]

    toMewlixStr level   Break = indent level (unwrapKeyword Keywords.run)
    toMewlixStr level   Continue = indent level (unwrapKeyword Keywords.catnap)
