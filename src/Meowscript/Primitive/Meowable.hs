{-# LANGUAGE TupleSections #-}

module Meowscript.Primitive.Meowable
( Meowable(..)
) where

import Meowscript.Primitive.Prim -- Importing everything intentionally!
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Meowscript.Primitive.MeowKey
import Data.IORef (newIORef)
import Data.Int (Int32)

class Meowable a where
    -- Lift value to MeowPrim.
    toMeow :: a -> IO MeowPrim

instance Meowable MeowPrim where
    toMeow = return

{- Useful Instances -}
----------------------------------------------------------------
instance Meowable Int where
    toMeow = return . MeowInt . fromIntegral

instance Meowable Int32 where
    toMeow = return . MeowInt

instance Meowable Double where
    toMeow = return . MeowFloat

instance Meowable Text.Text where
    toMeow x = (return . MeowString) $ BoxedString
        { unboxStr = x
        , strLen = Text.length x }

instance Meowable Bool where
    toMeow = return . MeowBool

instance Meowable Char where
    toMeow = toMeow . Text.singleton

instance (Meowable a) => Meowable [a] where
    toMeow xs = mapM toMeow xs >>= \list -> (return . MeowList) $ BoxedList
        { unboxList = list
        , listLen = length list }

instance Meowable MeowPairs where
    toMeow xs = do
        let pack (k, p) = (k,) <$> newIORef p
        MeowBox . HashMap.fromList <$> mapM pack (getPairs xs)

instance (MeowKey k, Meowable a) => Meowable (HashMap.HashMap k a) where
    toMeow x = do
        let pack (k, p) = (toMeowKey k,) <$> (toMeow p >>= newIORef)
        MeowBox . HashMap.fromList <$> mapM pack (HashMap.toList x)
