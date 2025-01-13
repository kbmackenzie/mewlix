module Mewlix.Compiler
( TranspilerContext(..)
, Transpiler
, transpile
, compileJS
, emptyContext
) where

import Mewlix.Compiler.Run
    ( TranspilerContext(..)
    , Transpiler
    , transpile
    , compileJS
    )
import Mewlix.Compiler.Transpiler (emptyContext)
