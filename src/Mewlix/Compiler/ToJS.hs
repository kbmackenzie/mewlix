module Mewlix.Compiler.ToJS
( ToJS(..)
) where

import Data.Text (Text)
import Mewlix.Compiler.Transpiler (Transpiler)

class ToJS a where
    toJS :: a -> Transpiler Text
