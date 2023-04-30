{-# LANGUAGE OverloadedStrings #-}

import Meowscript
import qualified Data.Text.IO as TextIO

runtime' :: IO ()
runtime' = do
    let path = "meowscript_practice1.txt"
    putStrLn "About to run it..."
    tok <- runBasic path
    --print tok
    case tok of
        (Right x) -> print x
        (Left x) -> TextIO.putStr x

main :: IO ()
main = do runtime'
