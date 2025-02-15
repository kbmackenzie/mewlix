{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Parser.String
( parseString
, parseStringM
, parseYarnString
) where

import Mewlix.Parser.Type (Parser)
import Mewlix.Abstract.AST
    ( Primitive(..)
    , BinaryOp(..)
    , Expression(..)
    )
import Mewlix.Parser.Utils (lexeme)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (label)
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

{- String quotations: -}
----------------------------------------------------------------
data QuoteType = QuoteType
    { open      :: Parser ()
    , close     :: Parser ()
    , predicate :: Char -> Bool }

type CharacterParser a = (Char -> Bool) -> Parser a

withQuotes :: [QuoteType] -> CharacterParser a -> Parser a
withQuotes quotes parse = do
    let chooseQuoteType :: Parser QuoteType
        chooseQuoteType = Mega.choice . flip map quotes $ \q -> q <$ open q

    quote <- chooseQuoteType
    parse (predicate quote) <* lexeme (close quote)

{- Single-line strings: -}
----------------------------------------------------------------
stringChar :: (Char -> Bool) -> Parser Char
stringChar allowed = Mega.choice
    [ MChar.char '\\' >> fmap escapeChar Mega.anySingle
    , MChar.eol >> fail "Linebreak in string!"
    , Mega.satisfy allowed                              ]

stringQuotes :: [QuoteType]
stringQuotes =
    [ QuoteType
        { open  = char '"'
        , close = char '"'
        , predicate = (/= '"')
        }
    , QuoteType
        { open  = char '\''
        , close = char '\''
        , predicate = (/= '\'')
        }
    ]
    where char = void . MChar.char

parseString :: Parser Text
parseString = label "string" . withQuotes stringQuotes $
    fmap Text.pack . Mega.many . stringChar

{- Multi-line strings: -}
----------------------------------------------------------------
stringCharM :: (Char -> Bool) -> Parser Char
stringCharM allowed = Mega.choice
    [ MChar.char '\\' >> fmap escapeChar Mega.anySingle
    , Mega.satisfy allowed                              ]

stringQuotesM :: [QuoteType]
stringQuotesM =
    [ QuoteType
        { open  = string "\"\"\""
        , close = string "\"\"\""
        , predicate = (/= '"')
        }
    , QuoteType
        { open  = string "'''"
        , close = string "'''"
        , predicate = (/= '\'')
        }
    ]
    where string = void . MChar.string

parseStringM :: Parser Text
parseStringM = label "multiline string" . withQuotes stringQuotesM $
    \characterPredicate -> do
        void (Mega.optional MChar.eol)
        fmap Text.pack . Mega.many . stringCharM $ characterPredicate

{- String interpolation ('yarn string'): -}
----------------------------------------------------------------
stringToExpr :: Text -> Expression
stringToExpr = PrimitiveExpr . MewlixString

interpolate :: [Expression] -> Expression
interpolate []  = stringToExpr mempty
interpolate [x] = StringCoerce x
interpolate xs  = foldl1 (BinaryOperation StringConcat) xs

stringCharY :: (Char -> Bool) -> Parser Char
stringCharY allowed = Mega.choice
    [ MChar.char '\\' >> fmap escapeChar Mega.anySingle
    , MChar.eol >> fail "Linebreak in string!"
    , Mega.satisfy allowed                              ]

stringQuotesY :: [QuoteType]
stringQuotesY =
    [ QuoteType
        { open  = string ":3\""
        , close = char '"'
        , predicate = \c -> c /= '"' && c /= '['
        }
    , QuoteType
        { open  = string ":3'"
        , close = char '\''
        , predicate = \c -> c /= '\'' && c /= '['
        }
    ]
    where
        string = void . MChar.string
        char = void . MChar.char

parseYarnString :: Parser Expression -> Parser Expression
parseYarnString expression = label "yarn string" . withQuotes stringQuotesY $
    \allowed -> do
        let brackets :: Parser a -> Parser a
            brackets p = (lexeme . MChar.char) '[' *> p <* MChar.char ']'

        let stringText :: Parser Text
            stringText = (fmap Text.pack . Mega.some) (stringCharY allowed)

        let stringPiece :: Parser Expression
            stringPiece = Mega.choice
                [ stringToExpr <$> stringText
                , brackets expression         ]

        pieces <- Mega.many stringPiece 
        return (interpolate pieces)
