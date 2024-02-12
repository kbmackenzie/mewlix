module Mewlix.Compiler.Run
( Transpiler
, transpile
, TranspilerContext(..)
, runCompiler
, CompilerFunc
, CompilerOutput
, compileJS
) where

import Mewlix.Abstract.AST (YarnBall)
import Mewlix.Compiler.Transpiler
    ( Transpiler
    , transpile
    , TranspilerContext(..)
    )
import Mewlix.Compiler.Javascript.ToJavascript (ToJS(toJS))
import Mewlix.Parser (parseMewlix, FileContent, ParserError)
import Data.Text (Text)

type CompilerFunc = TranspilerContext -> FilePath -> FileContent -> Either ParserError CompilerOutput
type CompilerOutput = Text
type CompilerCallback = YarnBall -> Transpiler Text

runCompiler :: CompilerCallback -> CompilerFunc
runCompiler callback context = (fmap compile .) . parseMewlix
    where compile = transpile context . callback

compileJS :: CompilerFunc
compileJS = runCompiler toJS
