{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Expression
( exprL
, exprR
, declaration
, liftedExpr
) where

import Mewlix.Abstract.AST
import Mewlix.Parser.Utils
import Mewlix.Parser.Primitive
import Mewlix.Parser.Keywords
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

termL :: Parser Expression
termL = Identifier <$> parseKey

termR :: Parser Expression
termR = Mega.choice
    [ parens exprR
    , parseBox
    , parseList
    , PrimitiveExpr <$> parsePrim
    , Identifier    <$> parseName ]

exprL :: Parser Expression
exprL = makeExprParser termL operatorsL

exprR :: Parser Expression
exprR = makeExprParser termR operatorsR


{- Data -}
------------------------------------------------------------------------------------
parseList :: Parser Expression
parseList = ListExpression <$> bracketList exprR <?> "list"

parseBox :: Parser Expression
parseBox = do
    let parsePair :: Parser (Key, Expression)
        parsePair = do
            key <- parseName
            symbol ':'
            value <- exprR
            return (key, value)

    keyword meowBox
    BoxExpression <$> bracketList parsePair <?> "box"


{- Operators -}
------------------------------------------------------------------------------------
property :: Parser Expression
property = ObjectProperty <$> parseKey

dotOp :: Parser (Expression -> Expression)
dotOp = do
    Mega.try $ do
        symbol '.'
        Mega.notFollowedBy (symbol '.')
    flip DotExpression <$> property

boxOp :: Parser (Expression -> Expression)
boxOp = LookupExpression <$> brackets exprR

call :: Parser (Expression -> Expression)
call = do
    args <- parensList exprR
    return (FunctionCall args)

postfixes :: Parser (Expression -> Expression)
postfixes = foldr1 (flip (.)) <$> Mega.some (Mega.choice [ dotOp, boxOp, call ])

lambda :: Parser (Expression -> Expression)
lambda = do
    keyword meowLambda
    args <- parensList parseKey
    keyword "=>"
    return (LambdaExpression args)


{- Operator Tables -}
------------------------------------------------------------------------------------
type OperatorTable = [[Operator Parser Expression]]

operatorsL :: OperatorTable
operatorsL = [[ Postfix postfixes ]]

operatorsR :: OperatorTable
operatorsR =
    [
        [ Postfix postfixes                                                     ]
    ,   [ Prefix  (Increment                        <$ keyword meowPaw      )
        , Prefix  (Decrement                        <$ keyword meowClaw     )   ]
    ,   [ Postfix (UnaryOperation LengthLookup      <$ keyword "?!"         )   ] 
    ,   [ Prefix  (UnaryOperation ListPeek          <$ keyword meowPeek     )
        , InfixL  (ListPush                         <$ keyword meowPush     )
        , Prefix  (ListPop                          <$ keyword meowKnock    )   ]
    ,   [ InfixL  (BinaryOperation ListConcat       <$ keyword ".."         )   ]
    ,   [ InfixL  (BinaryOperation Power            <$ symbol '^'           )   ]
    ,   [ Prefix  (UnaryOperation Negation          <$ symbol '-'           )
        , Prefix  (UnaryOperation BooleanNot        <$ keyword meowNot      )   ]
    ,   [ InfixL  (BinaryOperation Multiplication   <$ symbol '*'           )
        , InfixL  (BinaryOperation Division         <$ symbol '/'           )
        , InfixL  (BinaryOperation Modulo           <$ symbol '%'           )   ]
    ,   [ InfixL  (BinaryOperation Addition         <$ symbol '+'           )
        , InfixL  (BinaryOperation Subtraction      <$ symbol '-'           )   ]
    ,   [ InfixL  (BinaryOperation LesserOrEqual    <$ keyword "<="         )
        , InfixL  (BinaryOperation GreaterOrEqual   <$ keyword ">="         )
        , InfixL  (BinaryOperation LessThan         <$ symbol '<'           )
        , InfixL  (BinaryOperation GreaterThan      <$ symbol '>'           )   ]
    ,   [ InfixL  (BinaryOperation Equal            <$ keyword "=="         )
        , InfixL  (BinaryOperation NotEqual         <$ keyword "!="         )   ]
    ,   [ InfixL  (BooleanAnd                       <$ keyword meowAnd      )   ]
    ,   [ InfixL  (BooleanOr                        <$ keyword meowOr       )   ]
    ,   [ Prefix  lambda                                                        ]
    ,   [ TernR   ((TernaryOperation <$ symbol ':') <$ symbol '?'           )   ]
    ,   [ InfixR  (Assignment                       <$ symbol '='           )   ]
    ]

{- Declaration -}
------------------------------------------------------------------------------------
declaration :: Parser (Key, Expression)
declaration = do
    let getValue :: Parser Expression
        getValue = Mega.choice
            [ symbol '=' >> exprR
            , return (PrimitiveExpr MewlixNil) ]

    keyword meowLocal
    name   <- parseName
    rvalue <- getValue
    return (name, rvalue)

{- Lifted Expr -}
------------------------------------------------------------------------------------
liftedExpr :: Parser LiftedExpression
liftedExpr = Mega.choice
    [ uncurry LiftDeclaration   <$> declaration
    , LiftExpression            <$> exprR        ]
