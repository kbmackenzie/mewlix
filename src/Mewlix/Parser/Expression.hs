{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Expression
( exprL
, exprR
, declaration
, liftedExpr
) where

import Mewlix.Abstract.AST
    ( Key
    , Primitive(..)
    , Expression(..)
    , LiftedExpression(..)
    , Params(..)
    , BinaryOp(..)
    , UnaryOp(..)
    )
import Mewlix.Parser.Utils
    ( Parser
    , keyword
    , symbol
    , longSymbol
    , parens
    , brackets
    , parensList
    , bracketList
    , wordSequence
    )
import Mewlix.Parser.Primitive (parseKey, parsePrim, parseName)
import Text.Megaparsec ((<?>))
import qualified Mewlix.Keywords.Constants as Keywords
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

    longSymbol Keywords.box
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
    longSymbol Keywords.lambda
    params <- Params <$> parensList parseKey
    longSymbol "=>"
    return (LambdaExpression params)


{- Operator Tables -}
------------------------------------------------------------------------------------
type OperatorTable = [[Operator Parser Expression]]

operatorsL :: OperatorTable
operatorsL = [[ Postfix postfixes ]]

operatorsR :: OperatorTable
operatorsR =
    [
        [ Postfix postfixes                                                             ]
    ,   [ Prefix  (Increment                        <$ wordSequence Keywords.paw    )
        , Prefix  (Decrement                        <$ wordSequence Keywords.claw   )   ]
    ,   [ Postfix (UnaryOperation LengthLookup      <$ longSymbol "...?"            )   ] 
    ,   [ Prefix  (UnaryOperation ListPeek          <$ keyword Keywords.peek        )
        , InfixL  (ListPush                         <$ keyword Keywords.push        )
        , Prefix  (ListPop                          <$ wordSequence Keywords.pop    )   ]
    ,   [ InfixL  (BinaryOperation ListConcat       <$ longSymbol ".."              )   ]
    ,   [ InfixL  (BinaryOperation Power            <$ symbol '^'                   )   ]
    ,   [ Prefix  (UnaryOperation Negation          <$ symbol '-'                   )
        , Prefix  (UnaryOperation BooleanNot        <$ keyword Keywords.not         )   ]
    ,   [ InfixL  (BinaryOperation Multiplication   <$ symbol '*'                   )
        , InfixL  (BinaryOperation Division         <$ symbol '/'                   )
        , InfixL  (BinaryOperation Modulo           <$ symbol '%'                   )   ]
    ,   [ InfixL  (BinaryOperation Addition         <$ symbol '+'                   )
        , InfixL  (BinaryOperation Subtraction      <$ symbol '-'                   )   ]
    ,   [ InfixL  (BinaryOperation LesserOrEqual    <$ longSymbol "<="              )
        , InfixL  (BinaryOperation GreaterOrEqual   <$ longSymbol ">="              )
        , InfixL  (BinaryOperation LessThan         <$ symbol '<'                   )
        , InfixL  (BinaryOperation GreaterThan      <$ symbol '>'                   )   ]
    ,   [ InfixL  (BinaryOperation Equal            <$ longSymbol "=="              )
        , InfixL  (BinaryOperation NotEqual         <$ longSymbol "!="              )   ]
    ,   [ InfixL  (BooleanAnd                       <$ keyword Keywords.and         )   ]
    ,   [ InfixL  (BooleanOr                        <$ keyword Keywords.or          )   ]
    ,   [ Prefix  lambda                                                                ]
    ,   [ TernR   ((TernaryOperation <$ symbol ':') <$ symbol '?'                   )   ]
    ,   [ InfixR  (Assignment                       <$ symbol '='                   )   ]
    ]

{- Declaration -}
------------------------------------------------------------------------------------
declaration :: Parser (Key, Expression)
declaration = do
    let getValue :: Parser Expression
        getValue = Mega.choice
            [ symbol '=' >> exprR
            , return (PrimitiveExpr MewlixNil) ]

    keyword Keywords.local
    name   <- parseName
    rvalue <- getValue
    return (name, rvalue)

{- Lifted Expr -}
------------------------------------------------------------------------------------
liftedExpr :: Parser LiftedExpression
liftedExpr = Mega.choice
    [ uncurry LiftDeclaration   <$> declaration
    , LiftExpression            <$> exprR        ]
