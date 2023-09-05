{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Meowscript.Abstract.Meow
import Meowscript.Data.Ref
import Meowscript.Abstract.State
import Meowscript.Abstract.Prettify (prettyMeow)
import Meowscript.Interpreter.API
import Meowscript.Interpreter.Interpret (ReturnValue(..))
import qualified Meowscript.Data.Stack as Stack
import Meowscript.IO.Print (printException, printTextLn)
import qualified Data.HashMap.Strict as HashMap

main :: IO ()
main = do
    let path = "fib.meows"
    let libs = Libraries { getLibs = Stack.empty }
    ma <- runFile path True [] libs
    case ma of
        (Left e) -> printException (showException e)
        (Right a) -> case a of
            (ReturnPrim p) -> prettyMeow p >>= printTextLn
            _              -> printTextLn "void"

