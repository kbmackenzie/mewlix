{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.ToJS
( ToJS(..)
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.String.Escape (escapeString)
import Mewlix.String.Utils ((|++), parens, quotes)
import Mewlix.Compiler.Transpiler (Transpiler)
import Mewlix.Abstract.AST (Primitive(..))
import Mewlix.Utils.Show (showT)

type IndentLevel = Int;

class ToJS a where
    toJS :: a -> Transpiler Text

instance ToJS Primitive where
    toJS (MewlixInt i)      = (return . parens . showT) i
    toJS (MewlixBool b)     = (return . parens . showT) b
    toJS (MewlixFloat f)    = (return . parens . showT) f
    toJS (MewlixString s)   = (return . parens . quotes . escapeString) s
    toJS MewlixNil          = return "(null)"
    toJS MewlixHome         = return "this"
    toJS MewlixSuper        = return "super"
