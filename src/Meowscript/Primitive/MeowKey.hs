module Meowscript.Primitive.MeowKey
( Key
, MeowKey(..)
) where

import qualified Data.Text as Text

type Key = Text.Text

class MeowKey a where
    toMeowKey   :: a -> Key
    fromMeowKey :: Key -> a

instance MeowKey Text.Text where
    toMeowKey = id
    fromMeowKey = id
