{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Expr
( parseExpr
, parseExpr'
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Meowscript.Parser.Keywords
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Control.Monad (void)

exprTerm :: Parser Expr
exprTerm = ((lexeme . parens) parseExpr <?> "parens"    )
    <|> (Mega.try (lexeme parseObject)  <?> "box"       )
    <|> (lexeme parseList               <?> "list"      )
    <|> (EPrim <$> lexeme parsePrim                     ) 

parseExpr :: Parser Expr
parseExpr = makeExprParser exprTerm operators

parseExpr' :: Parser Expr
parseExpr' = lexeme (whitespace >> parseExpr)

operators :: [[Operator Parser Expr]]
operators =
    [
        [ Prefix  (EUnop MeowYarn                   <$ trySymbol "~~"      ) ]
      , [ InfixL  (EDot                             <$ parseDotOp          ) ]
      , [ Postfix (Mega.try functionCall                                   ) ]
      , [ Prefix  (EUnop  MeowPeek                  <$ trySymbol meowPeek  )
        , InfixL  (EBinop MeowPush                  <$ trySymbol meowPush  )
        , Prefix  (EUnop  MeowKnockOver             <$ trySymbol meowKnock )
        , Postfix (EUnop  MeowLen                   <$ symbol "?"          )
        , InfixL  (EBinop MeowConcat                <$ trySymbol ".."      ) ]
      , [ Prefix  (EUnop MeowNegate                 <$ symbol "-"          )
        , Prefix  (EUnop MeowNot                    <$ trySymbol meowBap   ) ]
      , [ InfixL  (EBinop MeowMul                   <$ symbol "*"          )
        , InfixL  (EBinop MeowDiv                   <$ symbol "/"          ) ]
      , [ InfixL  (EBinop MeowAdd                   <$ symbol "+"          )
        , InfixL  (EBinop MeowSub                   <$ symbol "-"          ) ]
      , [ InfixL  (EBinop (MeowCompare [LT, EQ])    <$ trySymbol "<="      )
        , InfixL  (EBinop (MeowCompare [GT, EQ])    <$ trySymbol ">="      )
        , InfixL  (EBinop (MeowCompare [LT])        <$ symbol "<"          )
        , InfixL  (EBinop (MeowCompare [GT])        <$ symbol ">"          ) ]
      , [ InfixL  (EBinop (MeowCompare [EQ])        <$ trySymbol "=="      )
        , InfixL  (EBinop (MeowCompare [LT, GT])    <$ trySymbol "!="      ) ]
      , [ InfixL  (EBinop MeowAnd                   <$ trySymbol "and"     ) ]
      , [ InfixL  (EBinop MeowOr                    <$ trySymbol "or"      ) ]
      , [ InfixR  (EBinop MeowAssign                <$ symbol "="          ) ]
    ]

functionCall :: Parser (Expr -> Expr)
functionCall = lexeme $ do
    (void . MChar.char) '('
    args <- Mega.sepBy (whitespaceLn >> lexeme parseExpr) (MChar.char ',')
    (void . MChar.char) ')'
    return (ECall args)

parseList :: Parser Expr
parseList = do
    (void . lexemeLn . MChar.char) '['
    list <- Mega.sepBy (whitespaceLn >> lexemeLn parseExpr) (MChar.char ',')
    (void . MChar.char) ']'
    (return . EList) list

parseObject :: Parser Expr
parseObject = do
    (void . lexemeLn . MChar.char) '{'
    object <- Mega.sepBy (whitespaceLn >> lexemeLn parseKeyValue) (MChar.char ',')
    (void . MChar.char) '}'
    (return . EObject) object

parseKeyValue :: Parser (Key, Expr)
parseKeyValue = do
    key <- lexeme keyText
    (void . lexeme) (MChar.char ':')
    expression <- lexeme parseExpr
    return (key, expression)

parseDotOp :: Parser ()
parseDotOp = Mega.try (MChar.char '.' >> (Mega.notFollowedBy . MChar.char) '.')
