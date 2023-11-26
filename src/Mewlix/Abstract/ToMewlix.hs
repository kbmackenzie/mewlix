{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Abstract.ToMewlix
( ToMewlix(..)
, IndentLevel
) where

import Mewlix.Abstract.AST
import Mewlix.Abstract.String
import Mewlix.Parser.Keywords
import Mewlix.Utils.Show (showT)
import Mewlix.Data.Stack (Stack)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Mewlix.Data.Stack as Stack

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
    toMewlixStr level xs = do
        let makeItems :: (ToMewlix a) => [a] -> Text
            makeItems = Text.intercalate ",\n" . map (indent (level + 1) . toMewlixStr level)
        Text.concat
            [ "[\n"
            , makeItems xs
            , "\n"
            , indent level "]" ]

instance (ToMewlix a) => ToMewlix (Stack a) where
    toMewlixStr level = toMewlixStr level . Stack.toList

{- Mewlix AST -}
----------------------------------------------------
instance ToMewlix Primitive where
    toMewlixStr _ (MewlixInt n)    = toMewlix n
    toMewlixStr _ (MewlixString s) = toMewlix s
    toMewlixStr _ (MewlixFloat f)  = toMewlix f
    toMewlixStr _ (MewlixBool b)   = toMewlix b
    toMewlixStr _ MewlixNil        = meowNil
    toMewlixStr _ MewlixHome       = meowHome
    toMewlixStr _ MewlixSuper      = meowSuper

instance ToMewlix Expression where
    toMewlixStr level   (PrimitiveExpr prim) = toMewlixStr level prim

    toMewlixStr _       (Identifier a) = a
    toMewlixStr _       (ObjectProperty a) = a

    toMewlixStr level   (BooleanAnd a b) = spaceSep
        [ toMewlixStr level a
        , meowAnd
        , toMewlixStr level b ]

    toMewlixStr level   (BooleanOr a b) = spaceSep
        [ toMewlixStr level a
        , meowOr
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

        let makeLines :: Stack (Key, Expression) -> Stack Text
            makeLines = fmap (indent level . prettyPair)
        undefined

    toMewlixStr level   (Assignment a b) = spaceSep
        [ toMewlixStr level a
        , "="
        , toMewlixStr level b ]

    toMewlixStr level   (Increment a) = spaceSep
        [ meowPaw
        , toMewlixStr level a ]

    toMewlixStr level   (Decrement a) = spaceSep
        [ meowClaw
        , toMewlixStr level a ]

    toMewlixStr level   (ListPush a b) = spaceSep
        [ toMewlixStr level a
        , meowPush
        , toMewlixStr level b ]

    toMewlixStr level   (ListPop a) = spaceSep
        [ meowKnock, toMewlixStr level a ]

    toMewlixStr level   _ = undefined

instance ToMewlix BinaryOp where
instance ToMewlix UnaryOp where

{-data Expression =
      PrimitiveExpr         Primitive
    | Identifier            Key
    | ObjectProperty        Key
    | BooleanAnd            Expression Expression
    | BooleanOr             Expression Expression
    | BinaryOperation       BinaryOp Expression Expression
    | UnaryOperation        UnaryOp Expression
    | TernaryOperation      Expression Expression Expression
    | ListExpression        (Stack Expression)
    | BoxExpression         (Stack (Key, Expression))
    | Assignment            Expression Expression
    | Increment             Expression
    | Decrement             Expression
    | ListPush              Expression Expression
    | ListPop               Expression
    | LambdaExpression      Params Expression
    | FunctionCall          (Stack Expression) Expression
    | DotExpression         Expression Expression
    | LookupExpression      Expression Expression
    deriving (Show)


 -}
