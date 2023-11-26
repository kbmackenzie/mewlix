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

instance (ToMewlix a) => ToMewlix [a] where
    toMewlixStr level items = do
        let newLevel = level + 1
        let prettify :: (ToMewlix a) => [a] -> Text
            prettify = flattenLn . map (indent newLevel . toMewlixStr newLevel)
        lineSep [ "[", prettify items, indent level "]" ]

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
        , Keywords.mewAnd
        , toMewlixStr level b ]

    toMewlixStr level (BooleanOr a b) = spaceSep
        [ toMewlixStr level a
        , Keywords.mewOr
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

    toMewlixStr level (ListExpression xs) = toMewlixStr level xs

    toMewlixStr level (BoxExpression pairs) = do
        let newLevel = level + 1

        let prettyPair :: (Key, Expression) -> Text
            prettyPair (key, expr) = Text.concat
                [ key
                , ": "
                , toMewlixStr newLevel expr ]

        let prettify :: [(Key, Expression)] -> Text
            prettify = flattenLn . map (indent newLevel . prettyPair)

        let header = Keywords.box `Text.append` " ["
        lineSep [ header, prettify pairs, indent level "]" ]

    toMewlixStr level (Assignment a b) = spaceSep
        [ toMewlixStr level a
        , "="
        , toMewlixStr level b ]

    toMewlixStr level (Increment a) = spaceSep
        [ Keywords.paw
        , toMewlixStr level a ]

    toMewlixStr level (Decrement a) = spaceSep
        [ Keywords.claw
        , toMewlixStr level a ]

    toMewlixStr level (ListPush a b) = spaceSep
        [ toMewlixStr level a
        , Keywords.push
        , toMewlixStr level b ]

    toMewlixStr level (ListPop a) = spaceSep
        [ Keywords.pop
        , toMewlixStr level a ]

    toMewlixStr level (LambdaExpression params body) = spaceSep
        [ Keywords.lambda
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
        ListPeek        -> Keywords.peek
        BooleanNot      -> Keywords.mewNot
        LengthLookup    -> "?!"

instance ToMewlix Statement where
    toMewlixStr level   (ExpressionStatement expr) = indent level (toMewlixStr level expr)

    toMewlixStr level   (WhileLoop condition block) = do
        let header = Text.concat
                [ Keywords.while
                , (parens . toMewlixStr level) condition ]
        lineSep [ header, toMewlixStr level block, Keywords.end ]

    toMewlixStr level   (ForLoop (a, b, c) block) = do
        let (start, middle, end) = Keywords.takeDo
        let header = spaceSep
                [ start
                , parens (toMewlixStr level a)
                , middle
                , parens (toMewlixStr level b)
                , end
                , parens (toMewlixStr level c) ]
        lineSep [ header, toMewlixStr level block, Keywords.end ]

instance ToMewlix LiftedExpression where

{-data Statement =
      ExpressionStatement   Expression
    | WhileLoop             Expression Block
    | ForLoop               (LiftedExpression, Expression, Expression) Block
    | IfElse                Expression Block Block
    | FunctionDef           MewlixFunction
    | Declaration           Key Expression
    | ClassDef              MewlixClass
    | ImportStatement       FilePathT (Maybe Key)
    | Return                Expression
    | TryCatch              Block CatchBlock
    | Break 
    | Continue
    deriving (Show)

 -}
