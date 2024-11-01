module Mewlix.Compiler
( TranspilerContext(..)
, Transpiler
, CompilerFunc
, compileJS
, emptyContext
) where

import Mewlix.Compiler.Run
    ( TranspilerContext(..)
    , Transpiler
    , CompilerFunc
    , compileJS
    )
import Mewlix.Compiler.Transpiler (emptyContext)
