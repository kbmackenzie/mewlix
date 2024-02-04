{-# LANGUAGE LambdaCase #-}

module Mewlix.Parser.Run
( runParser
, parseRoot
, parseExpr
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
import qualified Data.Text as Text

runParser :: Parser a -> FilePath -> Text -> Either Text a
runParser parser path contents = case parse parser path contents of
    (Left e)  -> (Left . Text.pack . errorBundlePretty) e
    (Right a) -> Right a

parseRoot :: FilePath -> Text -> Either Text YarnBall
parseRoot = runParser root

parseExpr :: FilePath -> Text -> Either Text Expression
parseExpr = runParser expression
