{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Func
( funDef
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Meowscript.Parser.Expr
import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer

funDiv :: Parser a -> Parser a
funDiv = meowDiv "purr"

funName :: Parser Text.Text
funName = atomName <$> lexeme (whitespace >> parseAtom)

funArgs :: Parser [Text.Text]
funArgs = (lexeme . bars) $ do
    whitespace
    args <- Mega.sepBy (whitespace >> parseAtom) (MChar.char ',')
    whitespace
    return $ atomName <$> args

funDef :: Parser Statement
funDef = (lexeme . funDiv) $ do
    whitespace
    name <- funName
    args <- funArgs
    whitespace
    return $ SFuncDef name args []
