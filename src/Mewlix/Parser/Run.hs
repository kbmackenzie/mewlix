{-# LANGUAGE LambdaCase #-}

module Mewlix.Parser.Run
( runParser
, parseRoot
) where

import Mewlix.Abstract.AST
    ( Block(..)
    )
import Mewlix.Parser.Utils (Parser)
import Mewlix.Parser.Statement (root)
import Text.Megaparsec (parse, errorBundlePretty)
import Data.Text (Text)
import qualified Data.Text as Text

runParser :: Parser a -> FilePath -> Text -> Either Text a
runParser parser path contents = case parse parser path contents of
    (Left e)  -> (Left . Text.pack . errorBundlePretty) e
    (Right a) -> Right a

parseRoot :: FilePath -> Text -> Either Text Block
parseRoot = runParser root
