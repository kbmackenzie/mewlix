{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Mewlix.Parser.Run (parseExpr, parseRoot)
import System.Environment (getArgs)
import qualified Data.Text.IO as TextIO
import Mewlix.Utils.Show (showT)

textPrint :: (Show a) => Either Text a -> IO ()
textPrint x = case x of
    (Left e) -> TextIO.putStrLn e
    (Right a) -> (TextIO.putStrLn . showT) a

expression :: IO ()
expression = do
    path    <- head <$> getArgs
    content <- TextIO.readFile path
    let expr = parseExpr path content
    textPrint expr

root :: IO ()
root = do 
    path    <- head <$> getArgs
    content <- TextIO.readFile path
    let expr = parseRoot path content
    textPrint expr

main :: IO ()
main = do
    TextIO.putStrLn "\n\n"
    root --expression --putStrLn "todo"
