{-# LANGUAGE TupleSections #-}

module Mewlix.Abstract.Meowable
( Meowable(..)
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Data.Stack (Stack)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (MonadIO(..))

class Meowable a where
    -- Lift value to MeowPrim.
    toMeow :: (MonadIO m) => a -> m MeowPrim

instance Meowable MeowPrim where
    toMeow = return

{- Useful Instances -}
----------------------------------------------------------------
instance Meowable Int where
    toMeow = return . MeowInt

instance Meowable Double where
    toMeow = return . MeowFloat

instance Meowable Text where
    toMeow = return . MeowString . toBoxedString

instance Meowable Bool where
    toMeow = return . MeowBool

instance Meowable Char where
    toMeow = toMeow . Text.singleton

instance (Meowable a) => Meowable (Stack a) where
    toMeow xs = mapM toMeow xs >>= \stack -> (return . MeowStack . toBoxedStack) stack

instance (Meowable a) => Meowable [a] where
    toMeow xs = mapM toMeow xs >>= \list -> (return . MeowStack . listToBoxedStack) list

instance Meowable MeowPairs where
    toMeow xs = do
        let pack (k, p) = (k,) <$> newRef p
        ref <- newRef . HashMap.fromList =<< mapM pack (getPairs xs)
        let box = CatBox { getBox = ref }
        return (MeowBox box)

instance Meowable () where
    toMeow _ = return MeowNil
