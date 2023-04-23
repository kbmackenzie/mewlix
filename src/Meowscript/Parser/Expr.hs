{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Expr
( parseExpr
, manyExpr
, exprStmt
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

manyExpr :: Parser [Expr]
manyExpr = Mega.sepEndBy (whitespace >> parseExpr) MChar.newline --(symbol ";")

exprTerm :: Parser Expr
exprTerm = (lexeme' . parens) parseExpr
    <|> (EPrim <$> lexeme' parsePrim)
--    <|> (EWhitespace <$ whitespace <|> Mega.empty)

parseExpr :: Parser Expr
parseExpr = makeExprParser exprTerm operators

exprStmt :: Parser Statement
exprStmt = SExpr <$> manyExpr

operators :: [[Operator Parser Expr]]
operators =
    [
        [ Prefix  (EUnop MeowNegate  <$ trySymbol "-"      )
        , Prefix  (EUnop MeowNot     <$ trySymbol "hiss" ) ]
      , [ Prefix  (EUnop MeowYarn    <$ trySymbol "~~"   ) ]
      , [ Postfix (EUnop MeowYarnLen <$ trySymbol "~?"   ) ]
      , [ InfixL (EBinop MeowMul <$ trySymbol "*")
        , InfixL (EBinop MeowDiv <$ trySymbol "/") ]
      , [ InfixL (EBinop MeowAdd <$ trySymbol "+")
        , InfixL (EBinop MeowSub <$ trySymbol "-") ]
      , [ InfixL (EBinop MeowPoke   <$ trySymbol "poke!" )
        , InfixL (EBinop MeowConcat <$ trySymbol ".."    ) ]
      , [ InfixL (EBinop (MeowCompare [LT])     <$ trySymbol "<" )
        , InfixL (EBinop (MeowCompare [GT])     <$ trySymbol ">" )
        , InfixL (EBinop (MeowCompare [LT, EQ]) <$ trySymbol "<=")
        , InfixL (EBinop (MeowCompare [GT, EQ]) <$ trySymbol ">=") ]
      , [ InfixL (EBinop (MeowCompare [EQ])     <$ trySymbol "==")
        , InfixL (EBinop (MeowCompare [LT, GT]) <$ trySymbol "!=") ]
      , [ InfixL (EBinop MeowAnd    <$ trySymbol "nya"      ) ]
      , [ InfixL (EBinop MeowOr     <$ trySymbol "nyo"      ) ]
      , [ InfixL (EBinop MeowAssign <$ trySymbol "=^.x.^="  ) ]
    ]
