{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Abstract.Prettify
( showMeow
, prettyMeow
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import qualified Data.Text as Text
import qualified Meowscript.Data.Stack as Stack
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (MonadIO(..))
import Meowscript.Utils.Show
import Data.List (sortBy)
import Data.Function (on)

{- Pretty-Printing -}
-----------------------------------------------------------------------------
showMeow :: (MonadIO m) => MeowAtom -> m Text.Text
{-# INLINE showMeow #-}
showMeow (MeowInt n) = (return . showT) n
showMeow (MeowFloat f) = (return . showT) f
showMeow (MeowString s) = (return . unboxStr) s
showMeow (MeowBool b) = (return . showT) b
showMeow (MeowStack xs) = do
    items <- mapM showMeow $ (Stack.toList . unboxStack) xs
    (return . Text.concat) [ "[", Text.intercalate ", " items, "]" ]
showMeow (MeowBox x) = do
    let unpack (key, ref) = fmap (key,) $ readRef ref >>= showMeow
    let pretty (key, prim) = Text.concat [ key, ": ", prim ]
    items <- map pretty . sortBy (compare `on` fst) <$> mapM unpack (HashMap.toList x)
    (return . Text.concat) [ "[", Text.intercalate ", " items, "]" ]
showMeow (MeowFunc _) = return "<function>"
showMeow MeowNil = return "<nothing>"

prettyMeow :: (MonadIO m) => MeowAtom -> m Text.Text
{-# INLINE prettyMeow #-}
prettyMeow (MeowString s) = (return . showT . unboxStr) s
prettyMeow other = showMeow other
