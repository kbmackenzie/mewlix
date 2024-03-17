module Mewlix.Parser.YarnString
( yarnstring
) where

import Mewlix.Abstract.AST (Primitive(..), Expression(..), BinaryOp(..))
import Mewlix.Parser.Utils (Parser, symbol, braces)
import Mewlix.Parser.Primitive (escapeChar)
import Mewlix.Parser.Expression (expression)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Control.Monad (void)

data YarnString =
      StringPiece Text
    | ExpressionPiece Expression
    deriving (Show)

stringChar :: Parser Char
stringChar = Mega.choice
    [ MChar.char '\\' >> fmap escapeChar Mega.anySingle
    , MChar.newline >> fail "Linebreak in string!"
    , Mega.satisfy (/= '"')                             ]

stringPiece :: Parser YarnString
stringPiece = StringPiece . Text.pack <$> Mega.many stringChar

expressionPiece :: Parser YarnString
expressionPiece = ExpressionPiece <$> braces expression

yarnPiece :: Parser YarnString
yarnPiece = stringPiece <|> expressionPiece

yarnstring :: Parser Expression
yarnstring = do
    let quotation :: Parser ()
        quotation = (void . MChar.char) '"' <?> "quotation mark"

    pieces <- do
        symbol '$'
        quotation
        pieces <- Mega.some yarnPiece
        quotation
        return pieces

    let unroll :: YarnString -> Expression
        unroll (StringPiece s)      = (PrimitiveExpr . MewlixString) s
        unroll (ExpressionPiece e)  = e

    let join :: Expression -> Expression -> Expression
        join = BinaryOperation ListConcat

    let putTogether :: (Monad m) => [YarnString] -> m Expression
        putTogether = return . foldr1 join . map unroll

    putTogether pieces
