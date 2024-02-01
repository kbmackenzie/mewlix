{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Expression
( expression
, declaration
, prettyExpr
) where

import Mewlix.Abstract.AST
    ( Primitive(..)
    , Expression(..)
    , Arguments(..)
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
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Parser.Primitive
    ( parseKey
    , parsePrim
    , parseKey
    , parseParams
    )
import Text.Megaparsec ((<?>))
import qualified Mewlix.Keywords.Constants as Keywords
import qualified Text.Megaparsec as Mega
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

{- Left-hand, Right-hand -}
------------------------------------------------------------------------------------
termL :: Parser Expression
termL = Identifier <$> parseKey

termR :: Parser Expression
termR = Mega.choice
    [ parens expression
    , parseBox
    , parseList
    , parseSuper
    , parseMeet
    , PrimitiveExpr <$> parsePrim
    , Identifier    <$> parseKey  ]

exprL :: Parser Expression
exprL = makeExprParser termL operatorsL

exprR :: Parser Expression
exprR = makeExprParser termR operatorsR

expression :: Parser Expression
expression = Mega.choice
    [ parens expression
    , assignment
    , parseLambda
    , parseThrow
    , exprR             ]

{- Data -}
------------------------------------------------------------------------------------
parseList :: Parser Expression
parseList = ListExpression <$> bracketList expression <?> "list"

parseBox :: Parser Expression
parseBox = do
    let parsePair :: Parser (Key, Expression)
        parsePair = do
            key <- parseKey
            symbol ':'
            value <- expression
            return (key, value)

    longSymbol Keywords.box
    BoxExpression <$> bracketList parsePair <?> "box"

parseArguments :: Parser Arguments
parseArguments = Arguments <$> parensList expression

{- Clowder -}
------------------------------------------------------------------------------------
parseSuper :: Parser Expression
parseSuper = do
    args <- Mega.try $ do
        keyword Keywords.super
        parseArguments
    return (SuperCall args)

parseMeet :: Parser Expression
parseMeet = do
    keyword Keywords.new
    ClowderCreate <$> termR <*> parseArguments

{- Throw -}
------------------------------------------------------------------------------------
parseThrow :: Parser Expression
parseThrow = do
    keyword Keywords.throw
    ThrowError <$> expression

{- Postfixes -}
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
boxOp = flip LookupExpression <$> brackets expression

call :: Parser (Expression -> Expression)
call = do
    args <- parseArguments
    return (`FunctionCall` args)

postfixes :: Parser (Expression -> Expression)
postfixes = foldr1 (flip (.)) <$> Mega.some (Mega.choice [ dotOp, boxOp, call ])

{- Lambda -}
------------------------------------------------------------------------------------
parseLambda :: Parser Expression
parseLambda = do
    longSymbol Keywords.lambda
    params <- parseParams
    longSymbol "=>"
    LambdaExpression params <$> exprR

{- Assignment -}
------------------------------------------------------------------------------------
assignment :: Parser Expression
assignment = do
    key <- Mega.try $ do
        key <- exprL
        symbol '='
        return key
    Assignment key <$> expression

{- Operator Tables -}
------------------------------------------------------------------------------------
type OperatorTable = [[Operator Parser Expression]]

operatorsL :: OperatorTable
operatorsL = [[ Postfix postfixes ]]

operatorsR :: OperatorTable
operatorsR =
    [
        [ Postfix postfixes                                                             ]
    ,   [ Prefix  (PawType                          <$ wordSequence Keywords.paw    )
        , Prefix  (ClawEntries                      <$ wordSequence Keywords.claw   )   ]
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
    ,   [ TernR   ((TernaryOperation <$ symbol ':') <$ symbol '?'                   )   ]
    ]

{- Declaration -}
------------------------------------------------------------------------------------
declaration :: Parser (Key, Expression)
declaration = do
    let getValue :: Parser Expression
        getValue = Mega.choice
            [ symbol '=' >> expression
            , return (PrimitiveExpr MewlixNil) ]

    keyword Keywords.local
    name   <- parseKey
    rvalue <- getValue
    return (name, rvalue)

{- Pretty Expressions -}
------------------------------------------------------------------------------------
prettyExpr :: Parser Expression
prettyExpr = termR
