module Mewlix.REPL.Actions
( REPL
, Line(..)
, LineCommand(..)
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Abstract.State
import Data.Text (Text)
import Mewlix.Parser.AST (LiftedExpr)

type REPL a = Environment MeowPrim -> IO a
data Line = Meta LineCommand | Expression LiftedExpr

data LineCommand = LineCommand
    { commandName :: Text
    , commandArgs :: [Text]  }
