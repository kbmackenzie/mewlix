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
    transpileJS :: IndentLevel -> a -> Transpiler Text

    toJS :: a -> Transpiler Text
    toJS = transpileJS 0

instance ToJS Primitive where
    transpileJS _ (MewlixInt i)     = (return . parens . showT) i
    transpileJS _ (MewlixBool b)    = (return . parens . showT) b
    transpileJS _ (MewlixFloat f)   = (return . parens . showT) f
    transpileJS _ (MewlixString s)  = (return . parens . quotes . escapeString) s
    transpileJS _ MewlixNil         = return "(null)"
    transpileJS _ MewlixHome        = return "this"
    transpileJS _ MewlixSuper       = return "super"
