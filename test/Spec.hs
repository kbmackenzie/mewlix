{-# LANGUAGE OverloadedStrings #-}

import Mewlix.Parser.Run (parseExpr)
import System.Environment (getArgs)
import qualified Data.Text.IO as TextIO
import Mewlix.Utils.Show (showT)

textPrint :: (Show a) => a -> IO ()
textPrint = TextIO.putStrLn . showT

expression :: IO ()
expression = do
    path    <- head <$> getArgs
    content <- TextIO.readFile path
    let expr = parseExpr path content
    textPrint expr

main :: IO ()
main = do
    TextIO.putStrLn "\n\n"
    expression --putStrLn "todo"
