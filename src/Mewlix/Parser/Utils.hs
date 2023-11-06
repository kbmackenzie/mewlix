{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Utils
( Parser
, whitespace
, whitespaceLn
, lexeme
, lexemeMultiline
, parens
, braces
, brackets
, parensList
, bracketList
, keyword
, symbol
) where

import Mewlix.Parser.Keywords
import Mewlix.Data.ToString
import Mewlix.Data.Stack (Stack)
import qualified Mewlix.Data.Stack as Stack
import Data.Text (Text)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Void (Void)
import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Char (isAlphaNum, isSpace)

type Parser = Mega.Parsec Void Text

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "--"

blockComment :: Parser ()
blockComment = start >> void (Mega.manyTill Mega.anySingle end)
    -- Case-insensitive comment symbols:
    where (start, end) = bimap MChar.string' MChar.string' meowComment

spaceChar :: Parser ()
spaceChar = (void . Mega.choice)
    [ (void . Mega.choice . map MChar.char) [ ' ', '\t' ]
    , (void . MChar.string) "\\\n"                      ]

parseSpace :: Parser ()
parseSpace = (void . Mega.some) spaceChar

isSpaceLn :: Char -> Bool
isSpaceLn c = isSpace c || c == ';'

whitespace :: Parser ()
whitespace = Lexer.space parseSpace lineComment blockComment

parseSpaceLn :: Parser ()
parseSpaceLn = void $ Mega.takeWhile1P (Just "space") isSpaceLn

whitespaceLn :: Parser ()
whitespaceLn = Lexer.space parseSpaceLn lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

lexemeMultiline :: Parser a -> Parser a
lexemeMultiline = Lexer.lexeme whitespaceLn

betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars open close f = do
    let lexChar :: Char -> Parser ()
        lexChar = void . MChar.char

    (lexemeMultiline . lexChar) open
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
        comma = (lexemeMultiline . void . MChar.char) ','

    let sepByComma :: Parser a -> Parser (Stack a)
        sepByComma = fmap Stack.fromList . flip Mega.sepEndBy comma

    (between . sepByComma . lexemeMultiline) token

parensList :: Parser a -> Parser (Stack a)
parensList = commaList parens

bracketList :: Parser a -> Parser (Stack a)
bracketList = commaList brackets

keyword :: Text -> Parser ()
keyword = lexeme . void . MChar.string

symbol :: Char -> Parser ()
symbol = lexeme . void . MChar.char

{-
module Mewlix.Parser.Utils
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

import Mewlix.Parser.Keywords
import Mewlix.Data.ToString
import Data.Text (Text)
import qualified Data.HashSet as HashSet
import Text.Megaparsec ((<?>), MonadParsec)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Void (Void)
import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Char (isAlphaNum)

type Parser = Mega.Parsec Void Text

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

symbol :: Text -> Parser Text
symbol = Lexer.symbol whitespace

trySymbol :: Text -> Parser Text
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

keyword :: Text -> Parser ()
keyword k = lexeme . (<?> "keyword") $ do
    (void . MChar.string) k
    Mega.notFollowedBy (Mega.satisfy validKeyChar)

tryKeyword :: Text -> Parser ()
tryKeyword = Mega.try . keyword

validKeyChar :: Char -> Bool
validKeyChar c = isAlphaNum c || c `elem` ['\'', '_']

keyText :: Parser Text
keyText = do
    x <- Mega.takeWhile1P (Just "key") validKeyChar
    if x `HashSet.member` reservedKeywords
        then fail "Variable name cannot be a keyword!"
        else return x

specialSymbol :: Text -> Parser ()
specialSymbol = lexeme . void . MChar.string'
-}
