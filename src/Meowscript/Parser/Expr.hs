{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Parser.Expr
( exprTerm
, parseExpr
, parseDotOp
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

exprTerm :: Parser Expr
exprTerm = ((lexeme . parens) parseExpr   <?> "parens"    )
    <|> (lexeme parseBox                  <?> "box"       )
    <|> (lexeme parseList                 <?> "list"      )
    <|> (ExprPrim <$> lexeme parsePrim                    )
    <|> (lexeme parseKey                  <?> "key"       )

expr :: Parser Expr
expr = makeExprParser exprTerm operators

parseExpr :: Parser Expr
parseExpr = lexeme (whitespace >> expr)

operators :: [[Operator Parser Expr]]
operators =
    [
        [ Postfix chainedOps                                                       ]
      , [ Prefix  (ExprUnop  UnopPaw                    <$ tryKeyword meowPaw    )
        , Prefix  (ExprUnop  UnopClaw                   <$ tryKeyword meowClaw   ) ]
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
    flip ExprDotOp <$> lexeme exprTerm

parseBoxOp :: Parser (Expr -> Expr)
parseBoxOp = (Mega.try . lexeme . brackets) (flip ExprBoxAccess <$> parseExpr)

chainedOps :: Parser (Expr -> Expr)
chainedOps = foldr1 (flip (.)) <$> Mega.some (parseDotOp <|> parseCall <|> parseBoxOp)
