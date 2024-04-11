{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Expression
( expression
, declaration
) where

import Mewlix.Abstract.AST
    ( Primitive(..)
    , Expression(..)
    , BinaryOp(..)
    , UnaryOp(..)
    , Arguments(..)
    , Params(..)
    )
import Mewlix.Parser.Utils
    ( Parser
    , lexeme
    , symbol
    , parens
    , brackets
    , parensList
    , bracketList
    )
import Mewlix.Parser.Keyword (keyword)
import Mewlix.Parser.String (parseYarnString)
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Parser.Primitive
    ( parseKey
    , parsePrim
    , parseKey
    , parseParams
    )
import Text.Megaparsec ((<?>))
import Mewlix.Keywords.Types (LongSymbol(..), unwrapKeyword)
import qualified Mewlix.Keywords.LanguageKeywords as Keywords
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Data.Maybe (fromMaybe)
import Control.Monad (void)
import qualified Data.List as List

{- Left-hand, Right-hand -}
------------------------------------------------------------------------------------
termL :: Parser Expression
termL = Mega.choice
    [ home
    , Identifier <$> parseKey ]

termR :: Parser Expression
termR = Mega.choice
    [ parens expression
    , box
    , shelf
    , superCall
    , new
    , parseYarnString expression
    , PrimitiveExpr <$> parsePrim
    , Identifier    <$> parseKey  ]

exprL :: Parser Expression
exprL = makeExprParser termL operatorsL <?> "left-hand expression"

exprR :: Parser Expression
exprR = makeExprParser termR operatorsR <?> "right-hand expression"

expression :: Parser Expression
expression = Mega.choice
    [ assignment
    , meow
    , listen
    , lambda
    , throw_
    , exprR             ]
    <?> "expression"

{- Data -}
------------------------------------------------------------------------------------
shelf :: Parser Expression
shelf = ListExpression <$> bracketList expression <?> "list"

box :: Parser Expression
box = do
    let parsePair :: Parser (Key, Expression)
        parsePair = do
            key <- parseKey
            symbol ':'
            value <- expression
            return (key, value)

    keyword Keywords.box
    BoxExpression <$> bracketList parsePair <?> "box"

arguments :: Parser Arguments
arguments = Arguments <$> parensList expression

{- Composing + Piping -}
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
        funcCall = (. argument) . FunctionCall

    keyword Keywords.compose
    return $ \f g -> do
        let apply :: Expression -> Expression
            apply = funcCall g . funcCall f
        let x = unwrapKeyword Keywords.composeRef

        LambdaExpression (param x) $ apply (var x)

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

listen :: Parser Expression
listen = do
    keyword Keywords.listen
    ListenExpression <$> Mega.optional expression

{- Clowder -}
------------------------------------------------------------------------------------
home :: Parser Expression
home = PrimitiveExpr MewlixHome <$ keyword Keywords.home

superCall :: Parser Expression
superCall = do
    args <- do
        keyword Keywords.superCall
        fromMaybe mempty <$> Mega.optional arguments
    return (SuperCall args)

new :: Parser Expression
new = do
    keyword Keywords.new
    ClowderCreate <$> termR <*> arguments

{- Throw -}
------------------------------------------------------------------------------------
throw_ :: Parser Expression
throw_ = do
    keyword Keywords.throw
    pos     <- Mega.getSourcePos
    expr    <- expression
    return (ThrowError expr pos)

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
postfixes = foldr1 (flip (.)) <$> Mega.some (Mega.choice [ dot, lookup_, call ])

{- Lambda -}
------------------------------------------------------------------------------------
lambda :: Parser Expression
lambda = do
    keyword Keywords.lambda
    params <- parseParams
    keyword $ LongSymbol "=>"
    LambdaExpression params <$> expression

{- Assignment -}
------------------------------------------------------------------------------------
assignment :: Parser Expression
assignment = do
    key <- Mega.try $ do
        key <- exprL
        lexeme $ do
            (void . MChar.char) '='
            Mega.notFollowedBy (MChar.char '=')
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
    ,   [ Prefix  (AskType                          <$ keyword Keywords.typeOf      )
        , InfixL  (IsInstance                       <$ keyword Keywords.is          )
        , Prefix  (ClawEntries                      <$ keyword Keywords.claw        )   ]
    ,   [ Postfix (UnaryOperation LengthLookup      <$ keyword (LongSymbol "...?")  )   ] 
    ,   [ Prefix  (UnaryOperation ListPeek          <$ keyword Keywords.peek        )
        , Prefix  (UnaryOperation ListPop           <$ keyword Keywords.pop         )
        , InfixL  (BinaryOperation ListPush         <$ keyword Keywords.push        )   ]
    ,   [ InfixL  (BinaryOperation StringConcat     <$ keyword (LongSymbol "..")    )
        , InfixL  (BinaryOperation Contains         <$ keyword Keywords.in_         )   ]
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
    ,   [ InfixL  nand                                                                  ]
    ,   [ InfixL  nor                                                                   ]
    ,   [ TernR   ternary                                                               ]
    ,   [ InfixL  pipe                                                                  ]
    ,   [ InfixL  compose                                                               ]
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
