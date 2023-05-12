module Meowscript.Utils.Show
( showT
) where

import Meowscript.Core.AST (Prim)
import qualified Data.Text as Text

showT :: (Show a) => a -> Text.Text
{-# INLINE showT #-}
{-# SPECIALIZE showT :: Prim -> Text.Text #-}
showT = Text.pack . show
