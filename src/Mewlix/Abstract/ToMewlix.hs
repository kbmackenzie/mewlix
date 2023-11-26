{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Abstract.ToMewlix
( ToMewlix(..)
, IndentLevel
) where

import Mewlix.Abstract.AST
import Mewlix.Abstract.String
import qualified Mewlix.Parser.Keywords as Keywords
import Mewlix.Utils.Show (showT)
import Data.Text (Text)
import qualified Data.Text as Text

{- A class for types that can be turned to Mewlix code.
 - This can be used for decompilation! -}

{- Typeclass -}
----------------------------------------------------
type IndentLevel = Int

class ToMewlix a where
    toMewlixStr :: IndentLevel -> a -> Text

    toMewlix :: a -> Text
    toMewlix = toMewlixStr 0

{- Utils -}
----------------------------------------------------
spaceSep :: [Text] -> Text
spaceSep = Text.intercalate " "

indent :: IndentLevel -> Text -> Text
indent level = do
    let indentation = Text.pack (replicate level ' ')
    Text.append indentation

flatten :: [Text] -> Text
flatten = Text.intercalate ",\n"

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

instance (ToMewlix a) => ToMewlix [a] where
    toMewlixStr level items = do
        let prettify :: (ToMewlix a) => [a] -> Text
            prettify = flatten . map (indent (level + 1) . toMewlixStr level)
        Text.concat
            [ "[\n"
            , prettify items
            , "\n"
            , indent level "]" ]

{- Mewlix AST -}
----------------------------------------------------
instance ToMewlix Primitive where
    toMewlixStr _ (MewlixInt n)    = toMewlix n
    toMewlixStr _ (MewlixString s) = toMewlix s
    toMewlixStr _ (MewlixFloat f)  = toMewlix f
    toMewlixStr _ (MewlixBool b)   = toMewlix b
    toMewlixStr _ MewlixNil        = Keywords.nil
    toMewlixStr _ MewlixHome       = Keywords.home
    toMewlixStr _ MewlixSuper      = Keywords.super

instance ToMewlix Expression where
    toMewlixStr level   (PrimitiveExpr prim) = toMewlixStr level prim

    toMewlixStr _       (Identifier a) = a
    toMewlixStr _       (ObjectProperty a) = a

    toMewlixStr level   (BooleanAnd a b) = spaceSep
        [ toMewlixStr level a
        , Keywords.mewAnd
        , toMewlixStr level b ]

    toMewlixStr level   (BooleanOr a b) = spaceSep
        [ toMewlixStr level a
        , Keywords.mewOr
        , toMewlixStr level b ]

    toMewlixStr level    (BinaryOperation op a b) = spaceSep
        [ toMewlixStr level a
        , toMewlixStr level op
        , toMewlixStr level b ]

    toMewlixStr level   (UnaryOperation op a) = spaceSep
        [ toMewlixStr level op
        , toMewlixStr level a ]

    toMewlixStr level   (TernaryOperation condition a b) = spaceSep
        [ toMewlixStr level condition
        , "?"
        , toMewlixStr level a
        , ":"
        , toMewlixStr level b ]

    toMewlixStr level   (ListExpression xs) = toMewlixStr level xs

    toMewlixStr level   (BoxExpression pairs) = do
        let prettyPair :: (Key, Expression) -> Text
            prettyPair (key, expr) = Text.concat
                [ key
                , ": "
                , toMewlixStr level expr ]

        let prettify :: [(Key, Expression)] -> Text
            prettify = flatten . map (indent level . prettyPair)

        Text.concat
            [ Keywords.box
            , " [\n"
            , prettify pairs
            , "\n"
            , indent level "]" ]

    toMewlixStr level   (Assignment a b) = spaceSep
        [ toMewlixStr level a
        , "="
        , toMewlixStr level b ]

    toMewlixStr level   (Increment a) = spaceSep
        [ Keywords.paw
        , toMewlixStr level a ]

    toMewlixStr level   (Decrement a) = spaceSep
        [ Keywords.claw
        , toMewlixStr level a ]

    toMewlixStr level   (ListPush a b) = spaceSep
        [ toMewlixStr level a
        , Keywords.push
        , toMewlixStr level b ]

    toMewlixStr level   (ListPop a) = spaceSep
        [ Keywords.pop
        , toMewlixStr level a ]

    toMewlixStr level   _ = undefined

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
        ListPeek        -> Keywords.peek
        BooleanNot      -> Keywords.mewNot
        LengthLookup    -> "?!"
