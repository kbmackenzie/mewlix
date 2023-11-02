{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Mewlix.Abstract.Prettify
( showMeow
, prettyMeow
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Mewlix.Data.Stack as Stack
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (MonadIO(..))
import Mewlix.Utils.Show
import Data.List (sortBy)
import Data.Function (on)

{- Pretty-Printing -}
-----------------------------------------------------------------------------
showMeow :: (MonadIO m) => MeowPrim -> m Text
{-# INLINE showMeow #-}
showMeow (MeowInt n) = (return . showT) n
showMeow (MeowFloat f) = (return . showT) f
showMeow (MeowString s) = (return . unboxStr) s
showMeow (MeowBool b) = (return . showT) b
showMeow (MeowStack xs) = do
    items <- Stack.toList <$> mapM showMeow (unboxStack xs)
    (return . Text.concat) [ "[", Text.intercalate ", " items, "]" ]
showMeow (MeowBox x) = do
    let unpack (key, ref) = fmap (key,) $ readRef ref >>= showMeow
    let pretty (key, prim) = Text.concat [ key, ": ", prim ]
    pairs <- (fmap HashMap.toList . readRef . getBox) x
    items <- map pretty . sortBy (compare `on` fst) <$> mapM unpack pairs
    (return . Text.concat) [ "[", Text.intercalate ", " items, "]" ]
showMeow (MeowFunc _) = return "<function>"
showMeow (MeowIFunc _) = return "<inner-func>"
showMeow MeowNil = return "<nothing>"

prettyMeow :: (MonadIO m) => MeowPrim -> m Text
{-# INLINE prettyMeow #-}
prettyMeow (MeowString s) = (return . showT . unboxStr) s
prettyMeow other = showMeow other
