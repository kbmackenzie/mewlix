{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Meowscript.Abstract.Meow
import Meowscript.Data.Ref
import Meowscript.Abstract.State
import Meowscript.Abstract.Prettify (prettyMeow)
import Meowscript.Interpreter.API
import qualified Meowscript.Data.Stack as Stack
import Meowscript.IO.Print (printException, printTextLn)
import qualified Data.HashMap.Strict as HashMap
import Meowscript.Libraries.Base (baseLibrary)

main :: IO ()
main = do
    let path = "examples/tictactoe/main.meows"
    --let path = "loop.meows"
    baseLib <- baseLibrary
    let libs = Libraries { getLibs = Stack.singleton baseLib }
    ma <- runFile path True [] libs
    case ma of
        (Left e) -> printException (showException e)
        (Right a) -> case a of
            (ReturnPrim p) -> prettyMeow p >>= printTextLn
            _              -> printTextLn "void"

