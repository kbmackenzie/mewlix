{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Utils
( lexeme
, linebreak
, whitespace
, skipLines
, multiline
, parens
, braces
, brackets
, parensList
, bracketList
, isKeyChar
, symbol
, repeatChar
, betweenChoice
) where

import Mewlix.Parser.Type (Parser)
import Mewlix.Keywords.Types (LongSymbol(..))
import qualified Mewlix.Keywords.Constants as Keywords
import Text.Megaparsec (label)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)
import Data.Char (isSpace, isAlphaNum)

{- Comments: -}
----------------------------------------------------------------
lineComment :: Parser ()
lineComment = Lexer.skipLineComment "--"

blockComment :: Parser ()
blockComment = open >> Mega.skipManyTill Mega.anySingle close
    -- Case-insensitive comment symbols:
    where 
        commentSymbol = void . MChar.string' . unwrapSymbol
        open  = commentSymbol Keywords.commentOpen
        close = commentSymbol Keywords.commentClose

{- Newlines: -}
----------------------------------------------------------------
escapeNewline :: Parser ()
escapeNewline = Mega.try (MChar.char '\\' >> void MChar.eol)

{- Single-line spaces: -}
----------------------------------------------------------------
space :: Parser ()
space = (void . Mega.choice)
    [ MChar.hspace1
    , escapeNewline ]

whitespace :: Parser ()
whitespace = Lexer.space (Mega.skipSome space) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

{- Multi-line spaces: -}
----------------------------------------------------------------
isMultiLineSpace :: Char -> Bool
isMultiLineSpace c = isSpace c || c == ';'

parseMultiLineSpace :: Parser ()
parseMultiLineSpace = (void . Mega.choice)
    [ void (Mega.satisfy isMultiLineSpace)
    , escapeNewline ]

skipLines :: Parser ()
skipLines = Lexer.space (Mega.skipSome parseMultiLineSpace) lineComment blockComment

multiline :: Parser a -> Parser a
multiline = Lexer.lexeme skipLines

linebreak :: Parser ()
linebreak = label "linebreak" . multiline . Mega.choice $
    [ void (MChar.char ';')
    , void MChar.eol
    , Mega.eof
    , fail "expected linebreak or semicolon!" ]

{- Lists: -}
----------------------------------------------------------------
betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars open close f = do
    let lexChar :: Char -> Parser ()
        lexChar = void . MChar.char

    (multiline . lexChar) open
    a <- f
    (lexeme . lexChar) close
    return a

parens :: Parser a -> Parser a
parens = betweenChars '(' ')'

braces :: Parser a -> Parser a
braces = betweenChars '{' '}'

brackets :: Parser a -> Parser a
brackets = betweenChars '[' ']'

commaList :: Parser a -> Parser [a]
commaList token = do
    let comma :: Parser ()
        comma = (multiline . void . MChar.char) ','

    let sepByComma :: Parser a -> Parser [a]
        sepByComma = flip Mega.sepEndBy comma

    (sepByComma . multiline) token

parensList :: Parser a -> Parser [a]
parensList = parens . commaList

bracketList :: Parser a -> Parser [a]
bracketList = brackets . commaList

{- Keywords/symbols: -}
----------------------------------------------------------------
isKeyChar :: Char -> Bool
isKeyChar c = isAlphaNum c || c == '_'

symbol :: Char -> Parser ()
symbol = lexeme . void . MChar.char

repeatChar :: Char -> Parser ()
repeatChar = Mega.skipSome . symbol

{- Extra -}
----------------------------------------------------------------
betweenChoice :: [(Parser (), Parser ())] -> Parser a -> Parser a
betweenChoice pairs parse = do
    let open :: [(Parser (), Parser ())] -> Parser (Parser ())
        open = Mega.choice . map (uncurry (<$))

    close <- open pairs
    parse <* close
