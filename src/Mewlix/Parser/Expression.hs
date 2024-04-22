{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Expression
( expression
, lvalue
, arguments
) where

import Mewlix.Abstract.AST
    ( Primitive(..)
    , Expression(..)
    , BinaryOp(..)
    , UnaryOp(..)
    , Arguments(..)
    , Params(..)
    )
import Mewlix.Parser.Type
    ( Parser
    , asks
    , nested
    , NestingFlag(..)
    )
import Mewlix.Parser.Utils
    ( lexeme
    , symbol
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
import Control.Monad (void, unless)
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
    , new
    , box
    , shelf
    , do_
    , parseYarnString expression
    , PrimitiveExpr <$> parsePrim
    , Identifier    <$> parseKey  ]

rvalue :: Parser Expression
rvalue = makeExprParser termL operatorsL <?> "left-hand expression"

lvalue :: Parser Expression
lvalue = makeExprParser termR operatorsR <?> "right-hand expression"

expression :: Parser Expression
expression = Mega.choice
    [ assignment
    , meow
    , listen
    , lambda
    , lvalue    ]
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

{- 'Do' Action -}
------------------------------------------------------------------------------------
do_ :: Parser Expression
do_ = do
    keyword Keywords.do_
    key  <- lvalue
    args <- fmap (maybe mempty Arguments) . Mega.optional $ do
        keyword Keywords.doArrow
        Mega.sepBy rvalue (symbol ',')
    return $ FunctionCall key args

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
home = do
    keyword Keywords.home
    inClass <- asks (nested InClass)
    unless inClass
        (fail "Cannot use clowder keyword outside clowder!")
    return $ PrimitiveExpr MewlixHome

new :: Parser Expression
new = do
    keyword Keywords.new
    ClowderCreate <$> termR <*> arguments

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
    params <- multiline parseParams
    multiline . keyword $ LongSymbol "=>"
    LambdaExpression params <$> expression

{- Assignment -}
------------------------------------------------------------------------------------
assignment :: Parser Expression
assignment = do
    key <- Mega.try $ do
        key <- rvalue
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
        , InfixL  (BinaryOperation FloorDivision    <$ keyword (LongSymbol "//")    )
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
