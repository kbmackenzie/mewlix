module Mewlix.Compiler.Run
( TranspilerContext(..)
, Transpiler
, compileJS
) where

import Mewlix.Abstract.AST (YarnBall)
import Mewlix.Compiler.Transpiler
    ( Transpiler
    , transpile
    , TranspilerContext(..)
    )
import Mewlix.Compiler.Javascript.ToJS (ToJS(toJS))
import Mewlix.Parser (parseMewlix)
import Data.Text (Text)

type CompilationFunction = YarnBall -> Transpiler Text

compileBase :: CompilationFunction -> TranspilerContext -> FilePath -> Text -> Either Text Text
compileBase callback context = (fmap compile .) . parseMewlix
    where compile = transpile context . callback

compileJS :: TranspilerContext -> FilePath -> Text -> Either Text Text
compileJS = compileBase toJS
