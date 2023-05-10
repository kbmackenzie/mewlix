{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Parser.RunParser
( meowParse
, exprParse
, parseLine
, specialLine
, exprLine
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Meowscript.Parser.Expr
import Meowscript.Parser.Statements
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Text.Megaparsec as Mega
import Text.Megaparsec.Error (errorBundlePretty)
import System.FilePath (takeFileName)

parseFile :: (Show a) => Parser a -> FilePath -> IO (Either Text.Text a)
parseFile parser path = do
    let fileName = takeFileName path
    contents <- TextIO.readFile path
    case Mega.parse parser fileName contents of
        (Right program) -> print program >> (return . Right) program
        (Left x) -> (return . Left . Text.pack . errorBundlePretty) x

meowParse :: FilePath -> IO (Either Text.Text [Statement])
meowParse = parseFile root

exprParse :: FilePath -> IO (Either Text.Text Expr)
exprParse = parseFile parseExpr'

parseLine :: Parser a -> Text.Text -> IO (Either Text.Text a)
parseLine parser str = case Mega.parse parser "<str>" str of
    (Right program) -> (return . Right) program
    (Left x) -> (return . Left . Text.pack . errorBundlePretty) x

specialLine :: Parser a -> Text.Text -> IO (Either Text.Text a)
specialLine = parseLine . Mega.between whitespaceLn Mega.eof

exprLine :: Text.Text -> IO (Either Text.Text Expr)
exprLine = specialLine (lexemeLn parseExpr)
