{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Utils
( Parser
, whitespace
, whitespaceLn
, lexeme
, lexemeLn
, parens
, braces
, brackets
, parensList
, bracketList
, keyword
, symbol
) where

import Mewlix.Parser.Keywords
import Mewlix.Data.Stack (Stack)
import qualified Mewlix.Data.Stack as Stack
import Data.Text (Text)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Void (Void)
import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Char (isSpace)

type Parser = Mega.Parsec Void Text

{- Comments: -}
----------------------------------------------------------------
lineComment :: Parser ()
lineComment = Lexer.skipLineComment "--"

blockComment :: Parser ()
blockComment = start >> void (Mega.manyTill Mega.anySingle end)
    -- Case-insensitive comment symbols:
    where (start, end) = bimap MChar.string' MChar.string' meowComment

{- Single-line spaces: -}
----------------------------------------------------------------
spaceChar :: Parser ()
spaceChar = (void . Mega.choice)
    [ (void . Mega.choice . map MChar.char) [ ' ', '\t' ]
    , (void . MChar.string) "\\\n"                      ]

manySpaces :: Parser ()
manySpaces = (void . Mega.some) spaceChar

whitespace :: Parser ()
whitespace = Lexer.space manySpaces lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

{- Multi-line spaces: -}
----------------------------------------------------------------
isSpaceLn :: Char -> Bool
isSpaceLn c = isSpace c || c == ';'

manySpacesLn :: Parser ()
manySpacesLn = void $ Mega.takeWhile1P (Just "space") isSpaceLn

whitespaceLn :: Parser ()
whitespaceLn = Lexer.space manySpacesLn lineComment blockComment

lexemeLn :: Parser a -> Parser a
lexemeLn = Lexer.lexeme whitespaceLn

{- Lists: -}
----------------------------------------------------------------
betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars open close f = do
    let lexChar :: Char -> Parser ()
        lexChar = void . MChar.char

    (lexemeLn . lexChar) open
    a <- f
    (lexeme . lexChar) close
    return a

parens :: Parser a -> Parser a
parens = betweenChars '(' ')'

braces :: Parser a -> Parser a
braces = betweenChars '{' '}'

brackets :: Parser a -> Parser a
brackets = betweenChars '[' ']'

commaList :: (Parser (Stack a) -> Parser (Stack a)) -> Parser a -> Parser (Stack a)
commaList between token = do
    let comma :: Parser ()
        comma = (lexemeLn . void . MChar.char) ','

    let sepByComma :: Parser a -> Parser (Stack a)
        sepByComma = fmap Stack.fromList . flip Mega.sepEndBy comma

    (between . sepByComma . lexemeLn) token

parensList :: Parser a -> Parser (Stack a)
parensList = commaList parens

bracketList :: Parser a -> Parser (Stack a)
bracketList = commaList brackets

{- Keywords/symbols: -}
----------------------------------------------------------------
keyword :: Text -> Parser ()
keyword = lexeme . void . MChar.string

symbol :: Char -> Parser ()
symbol = lexeme . void . MChar.char
