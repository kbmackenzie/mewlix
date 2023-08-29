{-# LANGUAGE TupleSections #-}

module Meowscript.Abstract.Meowable
( Meowable(..)
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Key
import Meowscript.Data.Ref
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int32)

class Meowable a where
    -- Lift value to MeowAtom.
    toMeow :: a -> IO MeowAtom

instance Meowable MeowAtom where
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
        let pack (k, p) = (k,) <$> newRef p
        MeowBox . HashMap.fromList <$> mapM pack (getPairs xs)

{-
instance (Meowable a) => Meowable (HashMap.HashMap Key a) where
    toMeow x = do
        let pack (k, p) = (k,) <$> (toMeow p >>= newRef)
        MeowBox . HashMap.fromList <$> mapM pack (HashMap.toList x)
-}
