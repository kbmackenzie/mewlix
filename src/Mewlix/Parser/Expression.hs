{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Expression
( expression
, declaration
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
    , symbol
    , parens
    , brackets
    , parensList
    , bracketList
    )
import Mewlix.Parser.Keyword (keyword)
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Parser.Primitive
    ( parseKey
    , parsePrim
    , parseKey
    , parseParams
    )
import Text.Megaparsec ((<?>))
import Mewlix.Keywords.Types (LongSymbol(..))
import qualified Mewlix.Keywords.Constants as Keywords
import qualified Text.Megaparsec as Mega
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

{- Left-hand, Right-hand -}
------------------------------------------------------------------------------------
termL :: Parser Expression
termL = Mega.choice
    [ parseHome
    , parseSuper
    , Identifier <$> parseKey ]

termR :: Parser Expression
termR = Mega.choice
    [ parens expression
    , parseBox
    , parseList
    , parseSuperCall
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
    , parseMeow
    , parseListen
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

    keyword Keywords.box
    BoxExpression <$> bracketList parsePair <?> "box"

parseArguments :: Parser Arguments
parseArguments = Arguments <$> parensList expression

{- IO -}
------------------------------------------------------------------------------------
parseMeow :: Parser Expression
parseMeow = do
    keyword Keywords.meow
    MeowExpression <$> expression

parseListen :: Parser Expression
parseListen = do
    keyword Keywords.listen
    ListenExpression <$> expression

{- Clowder -}
------------------------------------------------------------------------------------
parseHome :: Parser Expression
parseHome = PrimitiveExpr MewlixHome <$ keyword Keywords.home

parseSuper :: Parser Expression
parseSuper = PrimitiveExpr MewlixSuper <$ keyword Keywords.super

parseSuperCall :: Parser Expression
parseSuperCall = do
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
    pos     <- Mega.getSourcePos
    expr    <- expression
    return (ThrowError expr pos)

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
    keyword Keywords.lambda
    params <- parseParams
    keyword $ LongSymbol "=>"
    LambdaExpression params <$> exprR

{- Assignment -}
------------------------------------------------------------------------------------
assignment :: Parser Expression
assignment = do
    key <- Mega.try $ do
        key <- exprL
        symbol '='
        Mega.notFollowedBy (symbol '=')
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
    ,   [ Prefix  (PawType                          <$ keyword Keywords.paw             )
        , Prefix  (ClawEntries                      <$ keyword Keywords.claw        )   ]
    ,   [ Postfix (UnaryOperation LengthLookup      <$ keyword (LongSymbol "...?")  )   ] 
    ,   [ Prefix  (UnaryOperation ListPeek          <$ keyword Keywords.peek        )
        , InfixL  (ListPush                         <$ keyword Keywords.push        )
        , Prefix  (ListPop                          <$ keyword Keywords.pop         )   ]
    ,   [ InfixL  (BinaryOperation ListConcat       <$ keyword (LongSymbol "..")    )   ]
    ,   [ InfixL  (BinaryOperation Power            <$ symbol '^'                   )   ]
    ,   [ Prefix  (UnaryOperation Negation          <$ symbol '-'                   )
        , Prefix  (UnaryOperation BooleanNot        <$ keyword Keywords.not         )   ]
    ,   [ InfixL  (BinaryOperation Multiplication   <$ symbol '*'                   )
        , InfixL  (BinaryOperation Division         <$ symbol '/'                   )
        , InfixL  (BinaryOperation Modulo           <$ symbol '%'                   )   ]
    ,   [ InfixL  (BinaryOperation Addition         <$ symbol '+'                   )
        , InfixL  (BinaryOperation Subtraction      <$ symbol '-'                   )   ]
    ,   [ InfixL  (BinaryOperation LesserOrEqual    <$ keyword (LongSymbol "<=")    )
        , InfixL  (BinaryOperation GreaterOrEqual   <$ keyword (LongSymbol ">=")    )
        , InfixL  (BinaryOperation LessThan         <$ symbol '<'                   )
        , InfixL  (BinaryOperation GreaterThan      <$ symbol '>'                   )   ]
    ,   [ InfixL  (BinaryOperation Equal            <$ keyword (LongSymbol "==")    )
        , InfixL  (BinaryOperation NotEqual         <$ keyword (LongSymbol "!=")    )   ]
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
