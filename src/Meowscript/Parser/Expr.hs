{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Parser.Expr
( exprL
, exprR
, parseExpr
, declaration
, liftedExpr
) where

import Meowscript.Parser.AST
import Meowscript.Parser.Prim
import Meowscript.Parser.Utils
import Meowscript.Data.Stack (Stack)
import qualified Meowscript.Data.Stack as Stack
import Meowscript.Parser.Keywords
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Control.Monad (void)

type OperatorTable = [[Operator Parser Expr]]

termL :: Parser Expr
termL = lexeme parseKey

termR :: Parser Expr
termR = ((lexeme . parens) parseExpr      <?> "parens"    )
    <|> (lexeme parseBox                  <?> "box"       )
    <|> (lexeme parseList                 <?> "list"      )
    <|> (ExprPrim <$> lexeme parsePrim                    )
    <|> (lexeme parseKey                  <?> "key"       )

exprL :: Parser Expr
exprL = makeExprParser termL operatorsL

exprR :: Parser Expr
exprR = makeExprParser termR operatorsR

parseExpr :: Parser Expr
parseExpr = whitespace >> lexeme exprR

{- Operators -}
------------------------------------------------------------------------------------
operatorsL :: OperatorTable
operatorsL = [[ Postfix chainedOperators ]]

operatorsR :: OperatorTable
operatorsR =
    [
        [ Postfix chainedOperators                                                 ]
      , [ Prefix  (ExprPaw                              <$ tryKeyword meowPaw    )
        , Prefix  (ExprClaw                             <$ tryKeyword meowClaw   ) ]
      , [ Postfix (ExprUnop  UnopLen                    <$ trySymbol "?!"        ) ]
      , [ Prefix  (ExprUnop  UnopListPeek               <$ tryKeyword meowPeek   )
        , InfixL  (ExprBinop BinopListPush              <$ tryKeyword meowPush   )
        , Prefix  (ExprUnop  UnopListPop                <$ tryKeyword meowKnock  ) ]
      , [ InfixL  (ExprBinop BinopConcat                <$ trySymbol ".."        ) ]
      , [ InfixL  (ExprBinop BinopPow                   <$ trySymbol "^"         ) ]
      , [ Prefix  (ExprUnop  UnopNegate                 <$ symbol "-"            )
        , Prefix  (ExprUnop  UnopNot                    <$ tryKeyword meowNot    ) ]
      , [ InfixL  (ExprBinop BinopMul                   <$ symbol "*"            )
        , InfixL  (ExprBinop BinopDiv                   <$ symbol "/"            )
        , InfixL  (ExprBinop BinopMod                   <$ symbol "%"            ) ]
      , [ InfixL  (ExprBinop BinopAdd                   <$ symbol "+"            )
        , InfixL  (ExprBinop BinopSub                   <$ symbol "-"            ) ]
      , [ InfixL  (ExprBinop BinopCompareLEQ            <$ trySymbol "<="        )
        , InfixL  (ExprBinop BinopCompareGEQ            <$ trySymbol ">="        )
        , InfixL  (ExprBinop BinopCompareLess           <$ symbol "<"            )
        , InfixL  (ExprBinop BinopCompareGreat          <$ symbol ">"            ) ]
      , [ InfixL  (ExprBinop BinopCompareEq             <$ trySymbol "=="        )
        , InfixL  (ExprBinop BinopCompareNotEq          <$ trySymbol "!="        ) ]
      , [ InfixL  (ExprAnd                              <$ tryKeyword meowAnd    ) ]
      , [ InfixL  (ExprOr                               <$ tryKeyword meowOr     ) ]
      , [ Prefix  parseLambda                                                      ]
      , [ TernR   ((ExprTernary <$ symbol ":")          <$ trySymbol "?"         ) ]
      , [ InfixR  (ExprAssign                           <$ symbol "="            ) ]
    ]

parseList :: Parser Expr
parseList = (lexeme . brackets) $ whitespaceLn >>
    ExprList <$> sepByCommaEnd (bilexemeLn parseExpr) <* whitespaceLn

parseBox :: Parser Expr
parseBox = (Mega.try . lexemeLn . MChar.string') meowBox >> parseBox'

parseBox' :: Parser Expr
parseBox' = (lexeme . brackets) $ whitespaceLn >>
    ExprBox <$> sepByCommaEnd (bilexemeLn parseKeyValue) <* whitespaceLn

parseKeyValue :: Parser (Identifier, Expr)
parseKeyValue = do
    key <- lexeme keyText
    (void . lexeme) (MChar.char ':')
    expression <- lexeme parseExpr
    return (key, expression)

parseLambda :: Parser (Expr -> Expr)
parseLambda = do
    (void . Mega.try . lexeme . MChar.string') meowLambda
    let takeArgs = (sepByComma . bilexeme) keyText
    args <- Stack.fromList <$> (lexeme . parens) (whitespace >> takeArgs)
    (void . lexeme . MChar.string) "=>"
    return (ExprLambda args)

parseCall :: Parser (Expr -> Expr)
parseCall = (lexeme . parens . Mega.try) $ whitespaceLn >>
    flip ExprCall . Stack.fromList <$> sepByComma (bilexemeLn parseExpr) <* whitespaceLn

parseDotOp :: Parser (Expr -> Expr)
parseDotOp = do
    Mega.try $ do
        void (MChar.char '.')
        Mega.notFollowedBy (Mega.satisfy (== '.'))
    flip ExprDotOp <$> lexeme termR

parseBoxOp :: Parser (Expr -> Expr)
parseBoxOp = (Mega.try . lexeme . brackets) (flip ExprBoxAccess <$> parseExpr)

chainedOperators :: Parser (Expr -> Expr)
chainedOperators = foldr1 (flip (.)) <$> Mega.some (parseDotOp <|> parseCall <|> parseBoxOp)


{- Declaration -}
------------------------------------------------------------------------------------
declaration :: Parser (Identifier, Expr)
declaration = lexeme $ do
    (void . tryKeyword) meowLocal
    key <- lexeme keyText
    (void . symbol) "="
    (key,) <$> parseExpr


{- Lifted Expr -}
------------------------------------------------------------------------------------
liftedExpr :: Parser LiftedExpr
liftedExpr = (bilexeme . Mega.choice)
    [ uncurry LiftDecl  <$> declaration
    , LiftExpr          <$> exprR       ]
