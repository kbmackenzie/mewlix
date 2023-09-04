{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Evaluate.Evaluator
import Meowscript.Evaluate.State
import Meowscript.Evaluate.Exception
import Meowscript.Interpreter.Begin
import Meowscript.Abstract.Prettify (prettyMeow)
import Meowscript.Interpreter.Interpret (ReturnValue(..))
import Meowscript.Interpreter.Interpret
import Meowscript.Library.Base
import Meowscript.IO.Print (printException, printTextLn)

main :: IO ()
main = do
    baseLib <- baseLibrary
    let path = "fib.meows"
    let transforms = [ addLibrary baseLib ]
    let f = begin path (runFile path) transforms
    (fmap snd f) >>= \case
        (Left e) -> printException (showException e)
        (Right a) -> case a of
            (ReturnAtom p) -> prettyMeow p >>= printTextLn
            ReturnVoid     -> printTextLn "void"
            ReturnBreak    -> printTextLn "break"
