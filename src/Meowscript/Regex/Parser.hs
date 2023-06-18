{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Regex.Parser
( parseExpr
, parseRegex
) where

import Meowscript.Parser.Core (Parser)
import Meowscript.Regex.Core
import qualified Data.Text as Text
import Text.Megaparsec ((<|>))
import qualified Data.List as List
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import Data.Char (isDigit, isAlphaNum, isSpace)

parsePlus :: Parser (MeowRegex -> MeowRegex)
parsePlus = OneOrMore <$ MChar.char '+'

parseStar :: Parser (MeowRegex -> MeowRegex)
parseStar = ZeroOrMore <$ MChar.char '*'

parseQues :: Parser (MeowRegex -> MeowRegex)
parseQues = ZeroOrOne <$ MChar.char '?'

braces :: Parser a -> Parser a
braces = Mega.between (MChar.char '{') (MChar.char '}')

parseCount :: Parser (MeowRegex -> MeowRegex)
parseCount = Count <$> braces Lexer.decimal

parseRange :: Parser (MeowRegex -> MeowRegex)
parseRange = braces $ do
    start <- Mega.optional Lexer.decimal
    (void . MChar.char) ','
    end   <- Mega.optional Lexer.decimal
    return (CountRange start end)

parseNumRange :: Parser (MeowRegex -> MeowRegex)
parseNumRange = Mega.try parseCount <|> parseRange

postfixes :: [Parser (MeowRegex -> MeowRegex)]
postfixes = 
    [ parsePlus
    , parseStar
    , parseQues ]

--manyPostfixes :: Parser (MeowRegex -> MeowRegex)
--manyPostfixes = foldl1 (flip (.)) <$> Mega.some parsePostfix

parsePostfix :: Parser (MeowRegex -> MeowRegex)
parsePostfix = Mega.choice postfixes

reserved :: [Char]
reserved = [ '*', '.', '?', '+', '^', '$', '{', '(', ')', '[', ']', '|', '\\' ]

operators :: [Char]
operators = [ '*', '?', '+', '{' ]

isOperator :: Parser Bool
isOperator = True <$ Mega.satisfy (`elem` operators)

escapeChar :: [Char] -> Parser Char
escapeChar xs = Mega.choice
    [ '*'   <$ MChar.string "\\*"
    , '.'   <$ MChar.string "\\."
    , '?'   <$ MChar.string "\\?"
    , '+'   <$ MChar.string "\\+"
    , '^'   <$ MChar.string "\\^"
    , '$'   <$ MChar.string "\\$"
    , '('   <$ MChar.string "\\("
    , ')'   <$ MChar.string "\\)"
    , '['   <$ MChar.string "\\["
    , ']'   <$ MChar.string "\\]"
    , '|'   <$ MChar.string "\\|"
    , '-'   <$ MChar.string "\\-"
    , '\\'  <$ MChar.string "\\\\"
    , '{'   <$ (MChar.char '{' >> Mega.notFollowedBy MChar.digitChar)
    , Mega.satisfy (`notElem` xs) ]


{- Parse terms -}

parseLineStart :: Parser MeowRegex
parseLineStart = LineStart <$ MChar.char '^'

parseLineEnd :: Parser MeowRegex
parseLineEnd = LineEnd <$ MChar.char '$'

parseDot :: Parser MeowRegex
parseDot = AnyChar <$ MChar.char '.'

parseChar :: Parser MeowRegex
parseChar = Verbatim . Text.singleton <$> escapeChar reserved

parseVerbatim :: Parser MeowRegex
parseVerbatim = Verbatim . Text.pack <$> Mega.some char
    where char = Mega.try $ escapeChar reserved <* Mega.notFollowedBy isOperator

parseSpecial :: Parser MeowRegex
parseSpecial = Mega.choice
    [ Digit True            <$ MChar.string "\\d"
    , Digit False           <$ MChar.string "\\D"
    , WordChar True         <$ MChar.string "\\w"
    , WordChar False        <$ MChar.string "\\W"
    , Whitespace True       <$ MChar.string "\\s"
    , Whitespace False      <$ MChar.string "\\S" ]

{- Parsing character ranges (e.g. [a-Z]) -}

charRange :: Parser Predicate
charRange = do
    a <- escapeChar [ '\\', ']' ]
    (void . MChar.char) '-'
    b <- escapeChar [ '\\', ']' ]
    return $ Predicate (\x -> x `elem` [a..b])

isWordChar :: Char -> Bool
isWordChar x = isAlphaNum x || x == '_'

specialRange :: Parser Predicate
specialRange = Mega.choice
    [ Predicate isDigit             <$ MChar.string "\\d"
    , Predicate isSpace             <$ MChar.string "\\s"
    , Predicate isWordChar          <$ MChar.string "\\w"
    , Predicate (not . isDigit)     <$ MChar.string "\\D"
    , Predicate (not . isSpace)     <$ MChar.string "\\S"
    , Predicate (not . isWordChar)  <$ MChar.string "\\W" ]

brackets :: Parser a -> Parser a
brackets = Mega.between (MChar.char '[') (MChar.char ']')

parseCharRange :: Parser MeowRegex
parseCharRange = brackets $ do
    let predicates = Mega.try charRange <|> specialRange <|> charPred
    x <- Mega.optional $ MChar.char '^'
    (if isNothing x then AnyListed else AnyNotListed) <$> Mega.many predicates
    where char = Mega.try $ escapeChar [ '\\', ']' ] <* Mega.notFollowedBy (MChar.char '-')
          charPred = Predicate . (==) <$> char


{- Capture groups -}

parens :: Parser a -> Parser a
parens = Mega.between (MChar.char '(') (MChar.char ')')

parseGroup :: Parser MeowRegex
-- todo: parse group name
parseGroup = CaptureGroup Nothing <$> parens parseTokens


{- Combining everything: -}

parseTerm :: Parser MeowRegex
parseTerm = Mega.choice
    [ parseLineStart
    , parseLineEnd
    , parseCharRange
    , parseDot
    , parseGroup
    , parseSpecial
    , parseVerbatim
    , parseChar     ]

parseToken :: Parser MeowRegex
parseToken = do
    term  <- parseTerm
    p1    <- parseNumRange <|> return id
    p2    <- parsePostfix <|> return id
    (return . p2 . p1) term
    
parseAlt :: Parser ([MeowRegex] -> MeowRegex)
parseAlt = flip Alternation <$> (MChar.char '|' >> parseTokens)

parseTokens :: Parser [MeowRegex]
parseTokens = do
    xs <- Mega.many parseToken
    Mega.optional parseAlt >>= \case
        Nothing   -> return xs
        (Just fn) -> return [fn xs]

parseExpr :: Parser [MeowRegex]
parseExpr = parseTokens <* Mega.eof

parseRegex :: Text.Text -> Either Text.Text [MeowRegex]
parseRegex contents = case Mega.parse parseExpr "<regex>" contents of
    (Left x)  -> (Left . Text.pack . errorBundlePretty) x
    (Right x) -> Right x
