module Mewlix.Compiler
( Transpiler
, transpile
, TranspilerContext(..)
, toJS
) where

import Mewlix.Compiler.Javascript.ToJS (ToJS(toJS))
import Mewlix.Compiler.Transpiler
    ( transpile
    , Transpiler
    , TranspilerContext(..)
    )
