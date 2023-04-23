{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Meowscript.Core.Evaluate
(
) where

import Meowscript.Core.AST
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import qualified Control.Monad.Reader as Reader


