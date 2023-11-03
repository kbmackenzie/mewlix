{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Mewlix.Abstract.Prettify
( showMeow
, prettyMeow
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Data.Key (Key)
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
    let showPair :: (MonadIO m) => (Key, MeowPrim) -> m Text
        showPair (key, prim) = do
            valueStr <- showMeow prim
            return $ Text.concat [ key, ": ", valueStr ]

    let prettify :: (MonadIO m) => [(Key, MeowPrim)] -> m [Text]
        prettify = mapM showPair . sortBy (compare `on` fst) 

    items <- unpackBox x >>= prettify . HashMap.toList
    (return . Text.concat) [ "[", Text.intercalate ", " items, "]" ]
showMeow (MeowFunc _) = return "<function>"
showMeow (MeowIFunc _) = return "<inner-func>"
showMeow MeowNil = return "<nothing>"
showMeow _ = undefined

prettyMeow :: (MonadIO m) => MeowPrim -> m Text
{-# INLINE prettyMeow #-}
prettyMeow (MeowString s) = (return . showT . unboxStr) s
prettyMeow other = showMeow other
