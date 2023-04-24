{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Env
( baseEnv
) where

import Meowscript.Core.AST
import Meowscript.Core.Evaluate
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)

baseEnv :: Environment
baseEnv = Map.fromList
    [ ("meow"    , MeowFunc ["x"] meow    )
    , ("listen"  , MeowFunc []    listen  )]

meow :: [Statement]
meow = [ SExpr (EWrite (EPrim (MeowAtom "x"))) ]

listen :: [Statement]
listen = [ SReturn ERead ]
