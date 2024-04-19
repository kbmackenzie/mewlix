module Mewlix.Parser.Run
( ParserFunc
, ParseError
, FileContent
, runParser
, parseRoot
, parseExpr
, parseMewlix
) where

import Mewlix.Parser.Type (Parser, parse, FileContent, ParseError)
import Mewlix.Abstract.AST
    ( Expression
    , YarnBall
    )
import Mewlix.Parser.Statement (root)
import Mewlix.Parser.Expression (expression)

type ParserFunc a = FilePath -> FileContent -> Either ParseError a

runParser :: Parser a -> ParserFunc a
runParser parser path contents = parse path contents parser

parseRoot :: ParserFunc YarnBall
parseRoot = runParser root

parseExpr :: ParserFunc Expression
parseExpr = runParser expression

-- Alias:
parseMewlix :: ParserFunc YarnBall
parseMewlix = parseRoot
