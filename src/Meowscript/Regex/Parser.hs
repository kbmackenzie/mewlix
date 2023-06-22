{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Regex.Parser
( Parser
, parseExpr
, parseRegex
, isWordChar
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
import Control.Monad (void, liftM2)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Char (isDigit, isAlphaNum, isSpace)

parsePlus :: Parser (RegexAST -> RegexAST)
parsePlus = OneOrMore <$ MChar.char '+'

parseStar :: Parser (RegexAST -> RegexAST)
parseStar = ZeroOrMore <$ MChar.char '*'

parseQues :: Parser (RegexAST -> RegexAST)
parseQues = ZeroOrOne <$ MChar.char '?'

braces :: Parser a -> Parser a
braces = Mega.between (MChar.char '{') (MChar.char '}')

parseCount :: Parser (RegexAST -> RegexAST)
parseCount = Count <$> braces Lexer.decimal

parseRange :: Parser (RegexAST -> RegexAST)
parseRange = braces $ do
    start <- Mega.optional Lexer.decimal
    (void . MChar.char) ','
    end   <- Mega.optional Lexer.decimal
    return (CountRange start end)

parseNumRange :: Parser (RegexAST -> RegexAST)
parseNumRange = Mega.try parseCount <|> parseRange

postfixes :: [Parser (RegexAST -> RegexAST)]
postfixes = 
    [ parsePlus
    , parseStar
    , parseQues ]

--manyPostfixes :: Parser (RegexAST -> RegexAST)
--manyPostfixes = foldl1 (flip (.)) <$> Mega.some parsePostfix

parsePostfix :: Parser (RegexAST -> RegexAST)
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

parseLineStart :: Parser RegexAST
parseLineStart = LineStart <$ MChar.char '^'

parseLineEnd :: Parser RegexAST
parseLineEnd = LineEnd <$ MChar.char '$'

parseDot :: Parser RegexAST
parseDot = AnyChar <$ MChar.char '.'

parseChar :: Parser RegexAST
parseChar = Verbatim . Text.singleton <$> escapeChar reserved

parseVerbatim :: Parser RegexAST
parseVerbatim = Verbatim . Text.pack <$> Mega.some char
    where char = Mega.try $ escapeChar reserved <* Mega.notFollowedBy isOperator

parseSpecial :: Parser (Char -> Bool)
parseSpecial = Mega.choice
    [ isDigit              <$ MChar.string "\\d"
    , isSpace              <$ MChar.string "\\s"
    , isWordChar           <$ MChar.string "\\w"
    , (not . isDigit)      <$ MChar.string "\\D"
    , (not . isSpace)      <$ MChar.string "\\S"
    , (not . isWordChar)   <$ MChar.string "\\W" ]

parseSpecial' :: Parser RegexAST
parseSpecial' = CharacterClass . Predicate <$> parseSpecial

{- Parsing character ranges (e.g. [a-Z]) -}

charRange :: Parser (Char -> Bool)
charRange = do
    a <- escapeChar [ '\\', ']' ]
    (void . MChar.char) '-'
    b <- escapeChar [ '\\', ']' ]
    return (\x -> x `elem` [a..b])

isWordChar :: Char -> Bool
isWordChar x = isAlphaNum x || x == '_'

brackets :: Parser a -> Parser a
brackets = Mega.between (MChar.char '[') (MChar.char ']')

parseCharClass :: Parser RegexAST
parseCharClass = brackets $ do
    fns <- Mega.many (Mega.try charRange <|> parseSpecial <|> charPred)
    isNot <- isJust <$> Mega.optional (MChar.char '^')
    let predicate = foldr (liftM2 (||)) (const False) fns
    (return . CharacterClass . Predicate) (if isNot then not . predicate else predicate)
    where
        char = escapeChar [ '\\', ']' ] <* Mega.notFollowedBy (MChar.char '-')
        charPred = (==) <$> Mega.try char


{- Capture groups -}

parens :: Parser a -> Parser a
parens = Mega.between (MChar.char '(') (MChar.char ')')

parseGroup :: Parser RegexAST
-- todo: parse group name
parseGroup = CaptureGroup Nothing <$> parens parseTokens


{- Combining everything: -}

parseTerm :: Parser RegexAST
parseTerm = Mega.choice
    [ parseLineStart
    , parseLineEnd
    , parseCharClass
    , parseDot
    , parseGroup
    , parseSpecial'
    , parseVerbatim
    , parseChar     ]

parseToken :: Parser RegexAST
parseToken = do
    term  <- parseTerm
    p1    <- parseNumRange <|> return id
    p2    <- parsePostfix <|> return id
    (return . p2 . p1) term
    
parseAlt :: Parser ([RegexAST] -> RegexAST)
parseAlt = flip Alternation <$> (MChar.char '|' >> parseTokens)

parseTokens :: Parser [RegexAST]
parseTokens = do
    xs <- Mega.many parseToken
    Mega.optional parseAlt >>= \case
        Nothing   -> return xs
        (Just fn) -> return [fn xs]

parseExpr :: Parser [RegexAST]
parseExpr = parseTokens <* Mega.eof

parseRegex :: Text.Text -> Either Text.Text [RegexAST]
parseRegex contents = case Mega.parse parseExpr "<regex>" contents of
    (Left x)  -> (Left . Text.pack . errorBundlePretty) x
    (Right x) -> Right x
