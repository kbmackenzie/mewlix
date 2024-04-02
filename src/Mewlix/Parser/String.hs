{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.String
( parseString
, parseStringM
, parseYarnString
) where

import Mewlix.Abstract.AST
    ( Primitive(..)
    , BinaryOp(..)
    , Expression(..)
    )
import Mewlix.Parser.Utils
    ( Parser
    , lexeme
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Control.Monad (void)

{- Escape sequences: -}
----------------------------------------------------------------
escapeChar :: Char -> Char
escapeChar c = case c of
    'n'     -> '\n'
    't'     -> '\t'
    'b'     -> '\b'
    'r'     -> '\r'
    'f'     -> '\f'
    'v'     -> '\v'
    other   -> other

{- Single-line strings: -}
----------------------------------------------------------------
stringChar :: Parser Char
stringChar = Mega.choice
    [ MChar.char '\\' >> fmap escapeChar Mega.anySingle
    , MChar.newline >> fail "Linebreak in string!"
    , Mega.satisfy (/= '"')                             ]

parseString :: Parser Text
parseString = (<?> "string") $ do
    let quotation :: Parser ()
        quotation = (void . MChar.char) '"' <?> "quotation mark"

    quotation
    text <- Text.pack <$> Mega.many stringChar
    lexeme quotation
    return text

{- Multi-line strings: -}
----------------------------------------------------------------
stringCharM :: Parser Char
stringCharM = Mega.choice
    [ MChar.char '\\' >> fmap escapeChar Mega.anySingle
    , Mega.satisfy (/= '"')                             ]

parseStringM :: Parser Text
parseStringM = (<?> "multiline string") $ do
    let quotations :: Parser ()
        quotations = (void . MChar.string) "\"\"\"" <?> "triple quotes"

    quotations
    text <- Text.pack <$> Mega.many stringCharM
    lexeme quotations
    return text

{- String interpolation ('yarn string'): -}
----------------------------------------------------------------
stringToExpr :: Text -> Expression
stringToExpr = PrimitiveExpr . MewlixString

interpolate :: [Expression] -> Expression
interpolate = foldl (BinaryOperation StringConcat) emptyString
    where emptyString = stringToExpr mempty

stringCharI :: Parser Char
stringCharI = Mega.choice
    [ MChar.char '\\' >> fmap escapeChar Mega.anySingle
    , MChar.newline >> fail "Linebreak in string!"
    , Mega.satisfy (\x -> x /= '"' && x /= '[')        ]

parseYarnString :: Parser Expression -> Parser Expression
parseYarnString expression = (<?> "yarn string") $ do
    let quotation :: Parser ()
        quotation = (void . MChar.char) '"' <?> "quotation mark"

    let yarnMeow :: Parser ()
        yarnMeow = void (MChar.string' ":3\"" <?> "yarn string")

    let brackets :: Parser a -> Parser a
        brackets p = (lexeme . MChar.char) '[' *> p <* MChar.char ']'

    let stringPiece :: Parser Expression
        stringPiece = Mega.choice
            [ stringToExpr . Text.pack <$> Mega.some stringCharI
            , brackets expression                               ]

    yarnMeow
    pieces <- Mega.many stringPiece 
    quotation
    return (interpolate pieces)
