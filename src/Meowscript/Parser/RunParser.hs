{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Parser.RunParser
( meowParse
, exprParse
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

parseBase :: Parser a -> FilePath -> IO (Either Text.Text a)
parseBase parser path = do
    let fileName = takeFileName path
    contents <- TextIO.readFile path
    case Mega.parse parser fileName contents of
        (Right program) -> (return . Right) program
        (Left x) -> (return . Left . Text.pack . errorBundlePretty) x

meowParse :: FilePath -> IO (Either Text.Text [Statement])
meowParse = parseBase root

exprParse :: FilePath -> IO (Either Text.Text Expr)
exprParse = parseBase parseExpr'
