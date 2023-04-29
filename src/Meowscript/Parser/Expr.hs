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
import Control.Monad (void)

exprTerm :: Parser Expr
exprTerm = (lexeme . parens) parseExpr
    <|> lexeme parseList
    <|> lexeme parseObject
    <|> (EPrim <$> lexeme parsePrim)

parseExpr :: Parser Expr
parseExpr = makeExprParser exprTerm operators

parseExpr' :: Parser Expr
parseExpr' = lexeme (whitespace >> parseExpr)

operators :: [[Operator Parser Expr]]
operators =
    [
        [ Prefix  (EUnop MeowYarn    <$ trySymbol "~~"  ) ]
      , [ InfixL  (EDot              <$ parseDotOp      ) ]
      , [ Postfix $ Mega.try functionCall                 ]
      , [ Prefix  (EUnop MeowNegate  <$ symbol "-"      )
        , Prefix  (EUnop MeowNot     <$ trySymbol "bap" ) ]
      , [ InfixL  (EBinop MeowMul <$ symbol "*")
        , InfixL  (EBinop MeowDiv <$ symbol "/") ]
      , [ InfixL  (EBinop MeowAdd <$ symbol "+")
        , InfixL  (EBinop MeowSub <$ symbol "-") ]
      , [ Prefix  (EUnop MeowPoke   <$ trySymbol "poke"  )
        , Prefix  (EUnop MeowNudge  <$ trySymbol "nudge" )
        , Prefix  (EUnop MeowPeek   <$ trySymbol "peek"  )
        , Prefix  (EUnop MeowSneak  <$ trySymbol "sneak" )
        , Postfix (EUnop MeowLen   <$ symbol "~?"     )
        , InfixL  (EBinop MeowConcat <$ trySymbol ".."   ) ]
      , [ InfixL  (EBinop (MeowCompare [LT, EQ]) <$ trySymbol "<=")
        , InfixL  (EBinop (MeowCompare [GT, EQ]) <$ trySymbol ">=")
        , InfixL  (EBinop (MeowCompare [LT])     <$ symbol "<" )
        , InfixL  (EBinop (MeowCompare [GT])     <$ symbol ">" ) ]
      , [ InfixL  (EBinop (MeowCompare [EQ])     <$ trySymbol "==")
        , InfixL  (EBinop (MeowCompare [LT, GT]) <$ trySymbol "!=") ]
      , [ InfixL  (EBinop MeowAnd    <$ trySymbol "and"      ) ]
      , [ InfixL  (EBinop MeowOr     <$ trySymbol "or"       ) ]
      , [ InfixR  (EBinop MeowAssign <$ symbol "="           ) ]
    ]

functionCall :: Parser (Expr -> Expr)
functionCall = lexeme $ do
    void $ MChar.char '('
    x <- Mega.sepBy (whitespace >> lexeme parseExpr) (MChar.char ',')
    void $ MChar.char ')'
    return (ECall x)

parseList :: Parser Expr
parseList = do
    void $ MChar.char '['
    whitespaceLn
    list <- Mega.sepBy (whitespaceLn >> lexemeLn parseExpr) (MChar.char ',')
    void $ MChar.char ']'
    (return . EList) list

parseObject :: Parser Expr
parseObject = do
    void $ MChar.char '{'
    whitespaceLn
    obj <- Mega.sepBy (whitespaceLn >> lexemeLn parseKeyValue) (MChar.char ',')
    void $ MChar.char '}'
    (return . EObject) obj

parseKeyValue :: Parser (Key, Expr)
parseKeyValue = do
    key <- lexeme keyText
    (void . lexeme) (MChar.char ':')
    expr <- lexeme parseExpr
    return (key, expr)

parseDotOp :: Parser ()
parseDotOp = Mega.try $ MChar.char '.' >> Mega.notFollowedBy (MChar.char '.')
