module Mewlix.Parser.Run
( ParserFunc
, ParseError
, FileContent
, runParser
, root
, expression
, primitive
, parseMewlix
) where

import Mewlix.Parser.Type (Parser, parse, FileContent, ParseError)
import Mewlix.Abstract.AST (YarnBall)
import Mewlix.Parser.Statement (root)
import Mewlix.Parser.Expression (expression)
import Mewlix.Parser.Primitive (primitive)

type ParserFunc a = FilePath -> FileContent -> Either ParseError a

runParser :: Parser a -> ParserFunc a
runParser parser path contents = parse path contents parser

-- Alias:
parseMewlix :: ParserFunc YarnBall
parseMewlix = runParser root
