{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Core.Blocks
(
) where

import Meowscript.Core.AST
import Data.IORef
import Meowscript.Core.Environment
import Meowscript.Core.Pretty
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Reader (asks, ask, liftIO, local)
import Control.Monad.Except (throwError)
import Data.Functor ((<&>))
import Control.Monad (join)

evaluate :: Expr -> Evaluator Prim
evaluate (ExpPrim prim) = return prim
evaluate (ExpAssign a b) = do
    key <- asKey a
    value <- evaluate b

-- Dereference pointers.
ensureValue :: Prim -> Evaluator Prim
ensureValue (MeowKey x) = lookUp x
ensureValue x = return x

unwrapDots :: Expr -> Evaluator [Key]
unwrapDots (ExpTrail x y) = (:) <$> asKey x <*> unwrapDots y
unwrapDots x = List.singleton <$> asKey x

asKey :: Expr -> Evaluator Key
asKey (ExpPrim x) = showMeow' x
asKey (ExpYarn expr) = evaluate expr >>= showMeow'
asKey _ = throwError "Invalid key!"
