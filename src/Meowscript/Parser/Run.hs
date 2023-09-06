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
import Data.Text (Text)
import qualified Data.Text as Text

runParser :: Parser a -> FilePath -> Text -> Either Text a
runParser parser path contents = case parse parser path contents of
    (Left e)  -> (Left . Text.pack . errorBundlePretty) e
    (Right a) -> Right a

parseRoot :: FilePath -> Text -> Either Text Block
parseRoot = runParser root

parseLiftedExpr :: FilePath -> Text -> Either Text LiftedExpr
parseLiftedExpr = runParser liftedExpr
