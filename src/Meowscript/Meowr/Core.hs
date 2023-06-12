{-# LANGUAGE OverloadedStrings #-}

module Meowscript.API.Args
( MeowrArg(..)
) where

import Meowscript.Core.AST
import Meowscript.Utils.IO
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Char (isSpace)

data MeowrArg =
      MeowrString Text.Text
    | MeowrFlag Text.Text
    | MeowrOption Text.Text [Text.Text]
    deriving (Show)

instance Eq MeowrArg where
    MeowrString _   == MeowrString _   = True
    MeowrFlag   _   == MeowrFlag   _   = True
    MeowrOption _ _ == MeowrOption _ _ = True
    _ == _ = False
