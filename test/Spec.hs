{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Abstract.State
import Mewlix.Abstract.Prettify (prettyMeow)
import Mewlix.Interpreter.API
import qualified Mewlix.Data.Stack as Stack
import Mewlix.IO.Print (printException, printTextLn)
import qualified Data.HashMap.Strict as HashMap
import Mewlix.Libraries.Base (baseLibrary)

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

