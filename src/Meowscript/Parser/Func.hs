{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Parser.Func
( 
) where

import Meowscript.Core.AST
import Meowscript.Parser.Core
import Meowscript.Parser.Statements
import Meowscript.Parser.Expr
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad (void)
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer


