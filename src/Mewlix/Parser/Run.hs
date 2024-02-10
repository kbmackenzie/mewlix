module Mewlix.Parser.Run
( ParserFunc
, FileContent
, ParserError
, runParser
, parseRoot
, parseExpr
, parseMewlix
) where

import Mewlix.Abstract.AST
    ( Expression
    , YarnBall
    )
import Mewlix.Parser.Utils (Parser)
import Mewlix.Parser.Statement (root)
import Mewlix.Parser.Expression (expression)
import Text.Megaparsec (parse, errorBundlePretty)
import Data.Text (Text)

type ParserFunc a = FilePath -> FileContent -> Either ParserError a
type FileContent = Text
type ParserError = String

runParser :: Parser a -> ParserFunc a
runParser parser path contents = case parse parser path contents of
    (Left e)  -> (Left . errorBundlePretty) e
    (Right a) -> Right a

parseRoot :: ParserFunc YarnBall
parseRoot = runParser root

parseExpr :: ParserFunc Expression
parseExpr = runParser expression

-- Alias:
parseMewlix :: ParserFunc YarnBall
parseMewlix = parseRoot
