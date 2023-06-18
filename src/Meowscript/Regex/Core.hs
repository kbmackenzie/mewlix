{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Regex.Core
( MeowRegex(..)
) where

import Meowscript.Parser.Core (Parser)
import qualified Data.Text as Text
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Maybe (isNothing)

type GroupName = Maybe Text.Text
data MeowRegex =
      Verbatim Text.Text
    | AnyChar
    | LineStart
    | LineEnd
    | AnyListed [Char]
    | AnyNotListed [Char]
    | Count Int MeowRegex
    | CountRange (Maybe Int) (Maybe Int) MeowRegex
    | ZeroOrOne MeowRegex
    | ZeroOrMore MeowRegex
    | OneOrMore MeowRegex
    | Alternation MeowRegex MeowRegex
    | CaptureGroup GroupName MeowRegex

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

postfixes :: Parser (MeowRegex -> MeowRegex)
postfixes = Mega.choice
    [ parsePlus
    , parseStar
    , parseQues
    , parseNumRange ]

parsePostfixes :: Parser (MeowRegex -> MeowRegex)
parsePostfixes = foldl1 (flip (.)) <$> Mega.some postfixes

reservedChars :: [Char]
reservedChars = [ '*', '.', '?', '+', '^', '$', '{', '(', ')', '[', ']' ]

escapeChar :: Parser Char
escapeChar = Mega.choice
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
    , '-'   <$ MChar.string "\\-"
    , '\\'  <$ MChar.string "\\\\"
    , '{'   <$ (MChar.char '{' >> Mega.notFollowedBy MChar.digitChar)
    , Mega.satisfy (`notElem` reservedChars) ]


{- Parse terms -}

parseLineStart :: Parser MeowRegex
parseLineStart = LineStart <$ MChar.char '^'

parseLineEnd :: Parser MeowRegex
parseLineEnd = LineEnd <$ MChar.char '$'

parseVerbatim :: Parser MeowRegex
parseVerbatim = Verbatim . Text.pack <$> Mega.some escapeChar


{- Parsing character ranges (e.g. [a-Z]) -}

charRange :: Parser [Char]
charRange = do
    a <- escapeChar
    (void . MChar.char) '-'
    b <- escapeChar
    return [a..b]

brackets :: Parser a -> Parser a
brackets = Mega.between (MChar.char '[') (MChar.char ']')

parseCharRange :: Parser MeowRegex
parseCharRange = brackets $ do
    let chars = Mega.try charRange <|> Mega.some escapeChar
    x <- Mega.optional $ MChar.char '^'
    (if isNothing x then AnyListed else AnyNotListed) . concat <$> Mega.many chars


{- Combining everything: -}

parseTerm :: Parser MeowRegex
parseTerm = Mega.choice
    [ parseLineStart
    , parseLineEnd
    , parseCharRange
    , parseVerbatim ]

parseToken :: Parser MeowRegex
parseToken = do
    term <- parseTerm
    Mega.optional parsePostfixes >>= \case
        Nothing   -> return term
        (Just fn) -> return $ fn term
