module Meowscript.REPL.Actions
( REPL
, Line(..)
, LineCommand(..)
) where

import Meowscript.Abstract.Meow
import Meowscript.Data.Ref
import Meowscript.Abstract.State
import Data.Text (Text)
import Meowscript.Parser.AST (LiftedExpr)

type REPL a = Environment MeowPrim -> IO a
data Line = Meta LineCommand | Expression LiftedExpr

data LineCommand = LineCommand
    { commandName :: Text
    , commandArgs :: [Text]  }
