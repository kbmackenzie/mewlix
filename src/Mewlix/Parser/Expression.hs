{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Expression
( expression
, lvalue
, arguments
) where

import Mewlix.Abstract.AST
    ( Expression(..)
    , BinaryOp(..)
    , UnaryOp(..)
    , Arguments(..)
    , Params(..)
    )
import Mewlix.Parser.Type (Parser)
import Mewlix.Parser.Utils
    ( symbol
    , parens
    , brackets
    , parensList
    , bracketList
    , multiline
    )
import Mewlix.Parser.Keyword (keyword)
import Mewlix.Parser.String (parseYarnString)
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Parser.Primitive
    ( parseKey
    , home
    , outside
    , primitive
    , parseParams
    )
import Mewlix.Keywords.Shadow (shadow)
import Mewlix.Keywords.Types (LongSymbol(..))
import qualified Mewlix.Keywords.Constants as Keywords
import Text.Megaparsec ((<?>), label, hidden)
import qualified Text.Megaparsec as Mega
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import qualified Data.List as List
import Control.Arrow ((>>>))

{- Left-hand, Right-hand -}
------------------------------------------------------------------------------------
termL :: Parser Expression
termL = Mega.choice
    [ PrimitiveExpr <$> home
    , PrimitiveExpr <$> outside
    , Identifier <$> parseKey   ]

termR :: Parser Expression
termR = Mega.choice
    [ parens expression
    , new       <?> "new"
    , box       <?> "box"
    , shelf     <?> "shelf"
    , do_       <?> "do"
    , parseYarnString expression
    , PrimitiveExpr <$> primitive
    , Identifier    <$> parseKey  ]

lvalue :: Parser Expression
lvalue = makeExprParser termL operatorsL <?> "left-hand expression"

rvalue :: Parser Expression
rvalue = makeExprParser termR operatorsR <?> "right-hand expression"

expression :: Parser Expression
expression = Mega.choice
    [ meow
    , lambda
    , rvalue ]
    <?> "expression"

{- Operators -}
------------------------------------------------------------------------------------
-- Operator parsing util; literally just 'a <$ b' but with 'hidden' applied to b.
-- This keeps error messages from being polluted by operators.
(<&) :: a -> Parser b -> Parser a
(<&) a b = a <$ hidden b

{- Literals -}
------------------------------------------------------------------------------------
shelf :: Parser Expression
shelf = ShelfExpression <$> bracketList expression

box :: Parser Expression
box = do
    let parsePair :: Parser (Key, Expression)
        parsePair = do
            key <- parseKey
            symbol ':'
            value <- expression
            return (key, value)

    keyword Keywords.box
    BoxExpression <$> bracketList parsePair

{- Arguments -}
------------------------------------------------------------------------------------
arguments :: Parser Arguments
arguments = Arguments <$> parensList expression

arrowList :: Parser [Expression]
arrowList = do
    keyword Keywords.doArrow
    Mega.sepBy1 expression (symbol ',')

{- 'Do' Action -}
------------------------------------------------------------------------------------
do_ :: Parser Expression
do_ = do
    keyword Keywords.do_
    key  <- lvalue
    args <- maybe mempty Arguments <$> Mega.optional arrowList
    return $ FunctionCall key args

{- Composition + Piping -}
------------------------------------------------------------------------------------
pipe :: Parser (Expression -> Expression -> Expression)
pipe = do
    let argument = Arguments . List.singleton
    keyword Keywords.pipe
    return $ flip FunctionCall . argument

compose :: Parser (Expression -> Expression -> Expression)
compose = do
    let var = Identifier . Key
    let param = Params . List.singleton . Key
    let argument = Arguments . List.singleton

    let funcCall :: Expression -> Expression -> Expression
        funcCall = (argument >>>) . FunctionCall

    keyword Keywords.compose
    return $ \f g -> do
        let apply :: Expression -> Expression
            apply = funcCall g . funcCall f
        let ref = shadow "compose"
        LambdaExpression (param ref) $ apply (var ref)

{- Boolean -}
------------------------------------------------------------------------------------
nand :: Parser (Expression -> Expression -> Expression)
nand = do
    keyword Keywords.nand
    return $ (UnaryOperation BooleanNot .) . BooleanAnd

nor :: Parser (Expression -> Expression -> Expression)
nor = do
    keyword Keywords.nor
    return $ (UnaryOperation BooleanNot .) . BooleanOr

{- If/Else -}
------------------------------------------------------------------------------------
ternary :: Parser (Parser (Expression -> Expression -> Expression -> Expression))
ternary = let op = flip TernaryOperation
    in (op <$ keyword Keywords.ternElse) <$ keyword Keywords.ternIf

{- IO -}
------------------------------------------------------------------------------------
meow :: Parser Expression
meow = do
    keyword Keywords.meow
    MeowExpression <$> expression

{- Clowder -}
------------------------------------------------------------------------------------
new :: Parser Expression
new = do
    keyword Keywords.new
    ClowderCreate <$> lvalue <*> Mega.choice
        [ Arguments <$> arrowList
        , arguments               ]

{- Postfixes -}
------------------------------------------------------------------------------------
property :: Parser Expression
property = ObjectProperty <$> parseKey

dot :: Parser (Expression -> Expression)
dot = do
    Mega.try $ do
        symbol '.'
        Mega.notFollowedBy (symbol '.')
    flip DotExpression <$> property

lookup_ :: Parser (Expression -> Expression)
lookup_ = flip LookupExpression <$> brackets expression

call :: Parser (Expression -> Expression)
call = do
    args <- arguments
    return (`FunctionCall` args)

postfixes :: Parser (Expression -> Expression)
postfixes  = foldr1 (flip (.)) <$> (Mega.some . Mega.choice) [dot, lookup_, call]

lpostfixes :: Parser (Expression -> Expression)
lpostfixes = foldr1 (flip (.)) <$> (Mega.some . Mega.choice) [dot, lookup_]

{- Lambda -}
------------------------------------------------------------------------------------
lambda :: Parser Expression
lambda = label "lambda" $ do
    keyword Keywords.lambda
    params <- multiline parseParams
    multiline $ keyword Keywords.lambdaArrow
    LambdaExpression params <$> expression

{- Operator Tables -}
------------------------------------------------------------------------------------
type OperatorTable = [[Operator Parser Expression]]

operatorsL :: OperatorTable
operatorsL = [[ Postfix lpostfixes ]]

operatorsR :: OperatorTable
operatorsR =
    [
        [ Postfix postfixes                                                             ]
    ,   [ Prefix  (AskType                          <& keyword Keywords.typeOf      )
        , InfixL  (IsInstance                       <& keyword Keywords.is          )
        , Prefix  (ClawEntries                      <& keyword Keywords.claw        )
        , Postfix (UnaryOperation LengthLookup      <& keyword (LongSymbol "...?")  )   ] 
    ,   [ Prefix  (UnaryOperation ShelfPeek         <& keyword Keywords.peek        )
        , Prefix  (UnaryOperation ShelfPop          <& keyword Keywords.pop         )
        , InfixL  (BinaryOperation ShelfPush        <& keyword Keywords.push        )   ]
    ,   [ InfixL  (BinaryOperation StringConcat     <& keyword (LongSymbol "..")    )
        , InfixL  (BinaryOperation Contains         <& keyword Keywords.in_         )   ]
    ,   [ InfixL  (BinaryOperation Power            <& symbol '^'                   )   ]
    ,   [ Prefix  (UnaryOperation Plus              <& symbol '+'                   )
        , Prefix  (UnaryOperation Minus             <& symbol '-'                   )
        , Prefix  (UnaryOperation BooleanNot        <& keyword Keywords.not         )   ]
    ,   [ InfixL  (BinaryOperation Multiplication   <& symbol '*'                   )
        , InfixL  (BinaryOperation FloorDivision    <& keyword (LongSymbol "//")    )
        , InfixL  (BinaryOperation Division         <& symbol '/'                   )
        , InfixL  (BinaryOperation Modulo           <& symbol '%'                   )   ]
    ,   [ InfixL  (BinaryOperation Addition         <& symbol '+'                   )
        , InfixL  (BinaryOperation Subtraction      <& symbol '-'                   )   ]
    ,   [ InfixL  (BinaryOperation LesserOrEqual    <& keyword (LongSymbol "<=")    )
        , InfixL  (BinaryOperation GreaterOrEqual   <& keyword (LongSymbol ">=")    )
        , InfixL  (BinaryOperation LessThan         <& symbol '<'                   )
        , InfixL  (BinaryOperation GreaterThan      <& symbol '>'                   )   ]
    ,   [ InfixL  (BinaryOperation Equal            <& keyword (LongSymbol "==")    )
        , InfixL  (BinaryOperation NotEqual         <& keyword (LongSymbol "!=")    )   ]
    ,   [ InfixL  (BooleanAnd                       <& keyword Keywords.and         )   ]
    ,   [ InfixL  (BooleanOr                        <& keyword Keywords.or          )   ]
    ,   [ InfixL  nand                                                                  ]
    ,   [ InfixL  nor                                                                   ]
    ,   [ TernR   ternary                                                               ]
    ,   [ InfixL  pipe                                                                  ]
    ,   [ InfixL  compose                                                               ]
    ]
