{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Expr
( parseExpr
, parseExpr'
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Control.Monad (void, liftM)
import qualified Data.Text as Text

exprTerm :: Parser Expr
exprTerm = (lexeme . parens) parseExpr
    <|> (EPrim <$> lexeme parsePrim)

parseExpr :: Parser Expr
parseExpr = makeExprParser exprTerm operators

parseExpr' :: Parser Expr
parseExpr' = lexeme (whitespace >> parseExpr)

operators :: [[Operator Parser Expr]]
operators =
    [
        [ Postfix functionCall ]
      , [ Prefix  (EUnop MeowYarn    <$ trySymbol "~~"  ) ]
      , [ Prefix  (EUnop MeowNegate  <$ symbol "-"      )
        , Prefix  (EUnop MeowNot     <$ trySymbol "paw" ) ]
      , [ InfixL (EBinop MeowMul <$ symbol "*")
        , InfixL (EBinop MeowDiv <$ symbol "/") ]
      , [ InfixL (EBinop MeowAdd <$ symbol "+")
        , InfixL (EBinop MeowSub <$ symbol "-") ]
      , [ Prefix (EUnop MeowPoke   <$ trySymbol "poke"  )
        , Prefix (EUnop MeowNudge  <$ trySymbol "nudge" )
        , Prefix (EUnop MeowPeek   <$ trySymbol "peek"  )
        , Prefix (EUnop MeowSneak  <$ trySymbol "sneak" )
        , Postfix (EUnop MeowLen   <$ symbol "~?"     )
        , InfixL (EBinop MeowConcat <$ trySymbol ".."   ) ]
      , [ InfixL (EBinop (MeowCompare [LT, EQ]) <$ trySymbol "<=")
        , InfixL (EBinop (MeowCompare [GT, EQ]) <$ trySymbol ">=")
        , InfixL (EBinop (MeowCompare [LT])     <$ symbol "<" )
        , InfixL (EBinop (MeowCompare [GT])     <$ symbol ">" ) ]
      , [ InfixL (EBinop (MeowCompare [EQ])     <$ trySymbol "==")
        , InfixL (EBinop (MeowCompare [LT, GT]) <$ trySymbol "!=") ]
      , [ InfixL (EBinop MeowAnd    <$ trySymbol "nya"      ) ]
      , [ InfixL (EBinop MeowOr     <$ trySymbol "push"     ) ]
      , [ InfixL (EBinop MeowAssign <$ trySymbol "=^.x.^="  ) ]
    ]

functionCall :: Parser (Expr -> Expr)
functionCall = lexeme $ do
    void $ MChar.char '('
    x <- Mega.sepBy (whitespace >> lexeme parseExpr) (MChar.char ',')
    void $ MChar.char ')'
    return (ECall x)
