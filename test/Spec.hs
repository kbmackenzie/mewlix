{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Evaluate.Evaluator
import Meowscript.Evaluate.Environment
import Meowscript.Evaluate.State
import Meowscript.Evaluate.Exception
import Meowscript.Interpreter.Begin
import Meowscript.Abstract.Prettify (prettyMeow)
import Meowscript.Interpreter.Interpret
import Meowscript.Library.Base
import Meowscript.IO.Print (printException, printTextLn)
import qualified Data.HashMap.Strict as HashMap

main :: IO ()
main = do
    baseLib <- baseLibrary
    let path = "fib.meows"
    let transforms = [ addLibrary baseLib ]
    let f = begin path (librarySet >> runFile path) transforms
    (state, as) <- f
    --let ctx = evaluatorCtx state
    case as of
        (Left e) -> printException (showException e)
        (Right a) -> case a of
            (ReturnAtom p) -> prettyMeow p >>= printTextLn
            ReturnVoid     -> printTextLn "void"
            ReturnBreak    -> printTextLn "break"
