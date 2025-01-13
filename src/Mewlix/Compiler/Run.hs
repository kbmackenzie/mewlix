module Mewlix.Compiler.Run
( Transpiler
, transpile
, TranspilerContext(..)
, compileJS
) where

import Mewlix.Compiler.Transpiler
    ( Transpiler
    , transpile
    , TranspilerContext(..)
    )
import Mewlix.Compiler.JavaScript.ToJavaScript (ToJavaScript(..))
import Data.Text (Text)

compileJS :: (ToJavaScript a) => TranspilerContext -> a -> Text
compileJS context = transpile context . toJS
