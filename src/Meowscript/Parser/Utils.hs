{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Parser.Utils
( Parser
, (<??>)
, whitespace
, whitespaceLn
, lexeme
, bilexeme
, lexemeLn
, bilexemeLn
, symbol
, trySymbol
, parens
, quotes
, brackets
, comma
, sepByComma
, sepByCommaEnd
, keyword
, tryKeyword
, validKeyChar
, keyText
, specialSymbol
) where

import Meowscript.Parser.Keywords
import Meowscript.Data.ToString
import qualified Data.Text as Text
import qualified Data.HashSet as HashSet
import Text.Megaparsec ((<?>), MonadParsec)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Void (Void)
import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Char (isAlphaNum)

type Parser = Mega.Parsec Void Text.Text

-- A string-generic form of '<?>' for Haskell's many string types (String, Text, Bytestring).
infix 0 <??>
(<??>) :: (MonadParsec e s m, ToString b) => m a -> b -> m a
a <??> b = a <?> toString b

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "--"

blockComment :: Parser ()
blockComment = start >> void (Mega.manyTill Mega.anySingle end)
    -- Case-insensitive comment symbols:
    where (start, end) = bimap MChar.string' MChar.string' meowComment

spaceChars :: Parser ()
spaceChars = (void . Mega.some . Mega.choice) (MChar.string <$> [ " ", "\t", "\\\n" ])

whitespace :: Parser ()
whitespace = Lexer.space spaceChars lineComment blockComment

whitespaceLn :: Parser ()
whitespaceLn = Lexer.space MChar.space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

bilexeme :: Parser a -> Parser a
bilexeme a = whitespace >> lexeme a

lexemeLn :: Parser a -> Parser a
lexemeLn = Lexer.lexeme whitespaceLn

bilexemeLn :: Parser a -> Parser a
bilexemeLn a = whitespaceLn >> lexemeLn a

symbol :: Text.Text -> Parser Text.Text
symbol = Lexer.symbol whitespace

trySymbol :: Text.Text -> Parser Text.Text
trySymbol = lexeme . Mega.try . symbol

parens :: Parser a -> Parser a
parens = Mega.between (MChar.char '(') (MChar.char ')')

quotes :: Parser a -> Parser a
quotes = Mega.between (MChar.char '"') (MChar.char '"')

brackets :: Parser a -> Parser a
brackets = Mega.between (MChar.char '[') (MChar.char ']')

comma :: Parser Char
comma = MChar.char ','

commaLn :: Parser Char
commaLn = lexemeLn comma

sepByComma :: Parser a -> Parser [a]
sepByComma = flip Mega.sepBy comma

sepByCommaEnd :: Parser a -> Parser [a]
sepByCommaEnd = flip Mega.sepEndBy commaLn

keyword :: Text.Text -> Parser ()
keyword k = lexeme . (<?> "keyword") $ do
    (void . MChar.string) k
    Mega.notFollowedBy (Mega.satisfy validKeyChar)

tryKeyword :: Text.Text -> Parser ()
tryKeyword = Mega.try . keyword

validKeyChar :: Char -> Bool
validKeyChar c = isAlphaNum c || c `elem` ['\'', '_']

keyText :: Parser Text.Text
keyText = do
    x <- Mega.takeWhile1P (Just "key") validKeyChar
    if x `HashSet.member` reservedKeywords
        then fail "Variable name cannot be a keyword!"
        else return x

specialSymbol :: Text.Text -> Parser ()
specialSymbol = lexeme . void . MChar.string'
