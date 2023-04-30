{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Meowscript
import Meowscript.Core.AST
import Meowscript.Parser.Statements
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Error as MError
import Data.Void (Void)
import Control.StopWatch (stopWatch)

runParserExample :: String -> Text.Text -> Either (Mega.ParseErrorBundle Text.Text Void) Statement
runParserExample = Mega.parse root 

parseE :: Text.Text -> IO ()
parseE txt = do
    let !exp' = runParserExample "" txt
    putStrLn $ case exp' of
        (Left x) -> MError.errorBundlePretty x
        (Right x) -> show x

timedParsing :: IO ()
timedParsing = do 
    --let path = "meowscript_vector2.txt"
    let path = "meowscript_fibonacci.txt"
    !txt <- TextIO.readFile path
    (tok, time) <- stopWatch (parseE txt)
    print tok
    putStrLn ("Parsing time: " ++ show time)

runtime :: IO ()
runtime = do 
    --let path = "meowscript_vector2.txt"
    let path = "meowscript_fibonacci.txt"
    (tok, time) <- stopWatch (runBasic path)
    print tok
    putStrLn ("Runtime: " ++ show time)

main :: IO ()
main = do
    timedParsing
    runtime
