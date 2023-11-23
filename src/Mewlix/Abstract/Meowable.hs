{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Abstract.Meowable
( Meowable(..)
) where

import Mewlix.Abstract.AST
import Mewlix.Abstract.String
import Mewlix.Utils.Show (showT)
import Mewlix.Data.Stack (Stack)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Mewlix.Data.Stack as Stack

{- A class for types that can be turned to Mewlix code.
 - This can be used for decompilation! -}

class Meowable a where
    toMeow :: a -> Text

------------------------------------------------------
instance Meowable Text where
    toMeow = surround '"' . escapeString 

instance Meowable Int where
    toMeow = showT

instance Meowable Float where
    toMeow = showT

instance Meowable Bool where
    toMeow = Text.toLower . showT

instance (Meowable a) => Meowable [a] where
    toMeow xs = do
        let items :: (Meowable a) => [a] -> Text
            items = Text.intercalate ", " . map toMeow
        Text.concat [ "[ ", items xs, " ]" ]

instance (Meowable a) => Meowable (Stack a) where
    toMeow = toMeow . Stack.toList
