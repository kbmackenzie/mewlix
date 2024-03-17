module Mewlix.Parser.YarnString
( yarnstring
) where

import Mewlix.Abstract.AST (Primitive(..), Expression(..), BinaryOp(..))
import Mewlix.Parser.Utils (Parser, braces)
import Mewlix.Parser.Primitive (escapeChar)
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

expressionPiece :: Parser Expression -> Parser YarnString
expressionPiece = fmap ExpressionPiece . braces

yarnPiece :: Parser Expression -> Parser YarnString
yarnPiece parser = stringPiece <|> expressionPiece parser

-- Accept expression parser as an argument.
-- This makes this module not depend on Mewlix.Parser.Expression.
yarnstring :: Parser Expression -> Parser Expression
yarnstring parser = do
    let quotation :: Parser ()
        quotation = (void . MChar.char) '"' <?> "quotation mark"

    pieces <- do
        (void . MChar.char) '$'
        quotation
        pieces <- Mega.some (yarnPiece parser)
        quotation
        return pieces

    let unroll :: YarnString -> Expression
        unroll (StringPiece s)      = (PrimitiveExpr . MewlixString) s
        unroll (ExpressionPiece e)  = e

    let join :: Expression -> Expression -> Expression
        join = BinaryOperation StringConcat

    let putTogether :: (Monad m) => [YarnString] -> m Expression
        putTogether = return . foldr1 join . map unroll

    putTogether pieces
