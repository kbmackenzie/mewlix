{-# LANGUAGE LambdaCase #-}

module Meowscript.Parser.Run
( runParser
, parseRoot
, parseLiftedExpr
) where

import Meowscript.Parser.AST
import Meowscript.Parser.Utils (Parser)
import Meowscript.Parser.Statement (root)
import Meowscript.Parser.Expr (liftedExpr)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text as Text

runParser :: Parser a -> FilePath -> Text.Text -> Either Text.Text a
runParser parser path contents = case parse parser path contents of
    (Left e)  -> (Left . Text.pack . errorBundlePretty) e
    (Right a) -> Right a

parseRoot :: FilePath -> Text.Text -> Either Text.Text [Statement]
parseRoot = runParser root

parseLiftedExpr :: FilePath -> Text.Text -> Either Text.Text LiftedExpr
parseLiftedExpr = runParser liftedExpr
