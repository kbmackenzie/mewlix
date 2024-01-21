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
, isKeyChar
, keyword
, symbol
, longSymbol
, wordSequence
) where

import Mewlix.Keywords.Types
import Data.Text (Text)
import qualified Mewlix.Keywords.Constants as Keywords
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Void (Void)
import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Char (isSpace, isAlphaNum)

type Parser = Mega.Parsec Void Text

{- Comments: -}
----------------------------------------------------------------
lineComment :: Parser ()
lineComment = Lexer.skipLineComment "--"

blockComment :: Parser ()
blockComment = start >> void (Mega.manyTill Mega.anySingle end)
    -- Case-insensitive comment symbols:
    where (start, end) = bimap commentSymbol commentSymbol Keywords.comment
          commentSymbol = MChar.string' . unwrapSymbol

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

spaceCharLn :: Parser ()
spaceCharLn = (void . Mega.choice)
    [ void (Mega.satisfy isSpaceLn)
    , void (MChar.string "\\\n")    ]

manySpacesLn :: Parser ()
manySpacesLn = (void . Mega.some) spaceCharLn

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

commaList :: (Parser [a] -> Parser [a]) -> Parser a -> Parser [a]
commaList between token = do
    let comma :: Parser ()
        comma = (lexemeLn . void . MChar.char) ','

    let sepByComma :: Parser a -> Parser [a]
        sepByComma = flip Mega.sepEndBy comma

    (between . sepByComma . lexemeLn) token

parensList :: Parser a -> Parser [a]
parensList = commaList parens

bracketList :: Parser a -> Parser [a]
bracketList = commaList brackets

{- Keywords/symbols: -}
----------------------------------------------------------------
isKeyChar :: Char -> Bool
isKeyChar c = isAlphaNum c || c == '_'

keyword :: Keyword -> Parser ()
keyword key = (lexeme . Mega.try) $ do
    (void . MChar.string . unwrapKeyword) key
    Mega.notFollowedBy (Mega.satisfy isKeyChar)

symbol :: Char -> Parser ()
symbol = lexeme . void . MChar.char

longSymbol :: LongSymbol -> Parser ()
longSymbol = lexeme . void . MChar.string' . unwrapSymbol

wordSequence :: WordSequence -> Parser ()
wordSequence = mapM_ keyword . unwrapWords
