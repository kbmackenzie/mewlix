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
import Data.Char (isNumber)

exprTerm :: Parser Expr
exprTerm = ((lexeme . parens) parseExpr <?> "parens"    )
    <|> (lexeme parseBox                <?> "box"       )
    <|> (lexeme parseList               <?> "list"      )
    <|> (ExpPrim <$> lexeme parsePrim                     ) 

parseExpr :: Parser Expr
parseExpr = makeExprParser exprTerm operators

parseExpr' :: Parser Expr
parseExpr' = lexeme (whitespace >> parseExpr)

operators :: [[Operator Parser Expr]]
operators =
    [
        [ Prefix  (ExpYarn                            <$ trySymbol "~~"        ) ]
      , [ InfixL  (ExpTrail                           <$ parseDotOp            ) ]
      , [ Postfix (Mega.try functionCall                                       ) ]
      , [ Prefix  (ExpUnop  MeowPeek                  <$ tryKeyword meowPeek   )
        , InfixL  (ExpBinop MeowPush                  <$ tryKeyword meowPush   )
        , Prefix  (ExpUnop  MeowKnockOver             <$ tryKeyword meowKnock  )
        , Postfix (ExpUnop  MeowLen                   <$ symbol "?"            )
        , InfixL  (ExpBinop MeowConcat                <$ trySymbol ".."        ) ]
      , [ Prefix  (ExpUnop  MeowPaw                   <$ tryKeyword meowPaw    )
        , Prefix  (ExpUnop  MeowClaw                  <$ tryKeyword meowClaw   )
        , Prefix  (ExpUnop  MeowNegate                <$ symbol "-"            )
        , Prefix  (ExpUnop  MeowNot                   <$ tryKeyword meowNot    ) ]
      , [ InfixL  (ExpBinop MeowMul                   <$ symbol "*"            )
        , InfixL  (ExpBinop MeowDiv                   <$ symbol "/"            )
        , InfixL  (ExpBinop MeowMod                   <$ symbol "%"            ) ]
      , [ InfixL  (ExpBinop MeowAdd                   <$ symbol "+"            )
        , InfixL  (ExpBinop MeowSub                   <$ symbol "-"            ) ]
      , [ InfixL  (ExpBinop (MeowCompare [LT, EQ])    <$ trySymbol "<="        )
        , InfixL  (ExpBinop (MeowCompare [GT, EQ])    <$ trySymbol ">="        )
        , InfixL  (ExpBinop (MeowCompare [LT])        <$ symbol "<"            )
        , InfixL  (ExpBinop (MeowCompare [GT])        <$ symbol ">"            ) ]
      , [ InfixL  (ExpBinop (MeowCompare [EQ])        <$ trySymbol "=="        )
        , InfixL  (ExpBinop (MeowCompare [LT, GT])    <$ trySymbol "!="        ) ]
      , [ InfixL  (ExpBinop MeowAnd                   <$ tryKeyword "and"      ) ]
      , [ InfixL  (ExpBinop MeowOr                    <$ tryKeyword "or"       ) ]
      , [ Prefix  parseLambda                                                  ]
      , [ InfixR  (ExpBinop MeowAssign                <$ symbol "="            ) ]
    ]

functionCall :: Parser (Expr -> Expr)
functionCall = (lexeme . parens) $
    whitespace >>
    ExpCall <$> sepByComma (lnLexeme parseExpr)

parseList :: Parser Expr
parseList = (lexeme . brackets) $
    whitespaceLn >>
    ExpList <$> sepByComma (lnLexeme parseExpr)

parseBox :: Parser Expr
parseBox = (Mega.try . lexemeLn . MChar.string) meowBox >> parseBox'

parseBox' :: Parser Expr
parseBox' = (lexeme . brackets) $
    whitespaceLn >>
    ExpObject <$> sepByComma (lnLexeme parseKeyValue) <* whitespaceLn

parseKeyValue :: Parser (Key, Expr)
parseKeyValue = do
    key <- lexeme keyText
    (void . lexeme) (MChar.char ':')
    expression <- lexeme parseExpr
    return (key, expression)

parseLambda :: Parser (Expr -> Expr)
parseLambda = do
    (void . Mega.try . lexeme . MChar.string) meowLambda
    let takeArgs = (sepByComma . flexeme) keyText
    args <- (lexeme . parens) (whitespace >> takeArgs)
    (void . lexeme . MChar.string) "=>"
    return (ExpLambda args)

parseDotOp :: Parser ()
parseDotOp = Mega.try $ do
    void $ MChar.char '.'
    Mega.notFollowedBy $ Mega.satisfy (== '.')
