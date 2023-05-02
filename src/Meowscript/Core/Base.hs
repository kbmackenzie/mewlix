{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE LambdaCase #-}

module Meowscript.Core.Base
( baseLibrary
) where

import Meowscript.Core.AST
import Meowscript.Core.Environment
import Meowscript.Core.Exceptions
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Control.Monad.Except(throwError)
import Control.Monad.State(liftIO)
import Data.Functor((<&>))

baseLibrary :: EnvStack
baseLibrary = ((: []) . Map.fromList)
    [ ("meow"    , MeowIFunc  ["x"] meow      )
    , ("listen"  , MeowIFunc  [   ] listen    )
    , ("reverse" , MeowIFunc  ["x"] reverseFn ) ]

meow :: Evaluator Prim
meow = lookUpVar "x" >>= (liftIO . print) >> return MeowLonely

listen :: Evaluator Prim
listen = liftIO TextIO.getLine <&> MeowString

reverseFn :: Evaluator Prim
reverseFn = lookUpVar "x" >>= \case
    (MeowList x) -> (return . MeowList . reverse) x
    (MeowString x) -> (return . MeowString . Text.reverse) x
    x -> throwError (badIFunc "'reverse' can only be applied in lists and strings!" [x])
