{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Parser.RunParser
( meowParse
, exprParse
, parseText
, parseSpecial
, parseExprLine
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Meowscript.Parser.Expr
import Meowscript.Parser.Statements
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import Text.Megaparsec.Error (errorBundlePretty)
import System.FilePath (takeFileName)

parseFile :: (Show a) => Parser a -> FilePath -> Text.Text -> IO (Either Text.Text a)
parseFile parser path contents = case Mega.parse parser fileName contents of
        (Left x) -> (return . Left . Text.pack . errorBundlePretty) x
        (Right program) -> print program >> (return . Right) program
    where fileName = takeFileName path

meowParse :: FilePath -> Text.Text -> IO (Either Text.Text [Statement])
meowParse = parseFile root

exprParse :: FilePath -> Text.Text -> IO (Either Text.Text Expr)
exprParse = parseFile parseExpr'

parseText :: Parser a -> Text.Text -> Either Text.Text a
parseText parser str = case Mega.parse parser "<str>" str of
    (Right program) -> Right program
    (Left x) -> (Left . Text.pack . errorBundlePretty) x

parseSpecial :: Parser a -> Text.Text -> Either Text.Text a
parseSpecial = parseText . Mega.between whitespaceLn Mega.eof

parseExprLine :: Text.Text -> Either Text.Text Expr
parseExprLine = parseSpecial (lexemeLn parseExpr)
