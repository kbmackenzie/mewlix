module Mewlix.Compiler.Run
( Transpiler
, transpile
, TranspilerContext(..)
, runCompiler
, CompilerFunc
, compileJS
) where

import Mewlix.Abstract.AST (YarnBall)
import Mewlix.Compiler.Transpiler
    ( Transpiler
    , transpile
    , TranspilerContext(..)
    )
import Mewlix.Compiler.JavaScript.ToJavaScript (ToJavaScript(toJS))
import Mewlix.Parser (parseMewlix, FileContent, ParseError)
import Data.Text (Text)

type CompilerFunc = TranspilerContext -> FilePath -> FileContent -> Either ParseError Text
type CompilerCallback = YarnBall -> Transpiler Text

runCompiler :: CompilerCallback -> CompilerFunc
runCompiler callback context = (fmap compile .) . parseMewlix
    where compile = transpile context . callback

compileJS :: CompilerFunc
compileJS = runCompiler toJS
