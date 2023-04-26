{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Base
( baseLibrary
) where

import Meowscript.Core.AST
import Meowscript.Core.Evaluate
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)

baseLibrary :: EnvStack
baseLibrary = ((: []) . Map.fromList)
    [ ("meow"    , MeowFunc ["x"] meow    )
    , ("listen"  , MeowFunc []    listen  )]

meow :: [Statement]
meow = [ SExpr (EWrite (EPrim (MeowKey "x"))) ]

listen :: [Statement]
listen = [ SReturn ERead ]
