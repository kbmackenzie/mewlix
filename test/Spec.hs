{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Parser.Run
import Mewlix.Parser.Expr
import Mewlix.Parser.Statement
import Mewlix.Abstract.State
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MChar
import Mewlix.Abstract.Prettify (prettyMeow)
import Mewlix.Interpreter.API
import qualified Data.Text.IO as TextIO
import qualified Mewlix.Data.Stack as Stack
import Mewlix.IO.Print (printException, printTextLn)
import qualified Data.HashMap.Strict as HashMap
import Mewlix.Libraries.Base (baseLibrary)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
    {-
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
    -}
    let path = "example_meow.meows"
    --let path = "std/hashmap.meows"
    --let path = "std/io.meows" --"std/std.meows"
    -- contents <- TextIO.readFile path
    {-
    let parser = do
            Mega.many (whileLoop Root <|> ifelse Root)
            --Mega.many (exprR <* MChar.newline)
    let parsed = case Mega.parse root path contents of
            (Left e)  -> errorBundlePretty e
            (Right a) -> show a
    --let parsed = parseRoot path contents
    putStrLn parsed
    -}
    baseLib <- baseLibrary
    let libs = Libraries { getLibs = Stack.singleton baseLib }
    ma <- runFile path True [] libs
    case ma of
        (Left e) -> printException (showException e)
        (Right a) -> case a of
            (ReturnPrim p) -> prettyMeow p >>= printTextLn
            _              -> printTextLn "void"
