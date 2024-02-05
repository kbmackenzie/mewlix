{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Mewlix.Parser.Run (parseExpr, parseRoot)
import System.Environment (getArgs)
import qualified Data.Text.IO as TextIO
import Mewlix.Utils.Show (showT)
import Control.Monad (void)

import Mewlix.Abstract.AST
import Mewlix.Compiler.Javascript.ToJS
import Mewlix.Compiler.Javascript.Transpiler

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

root :: IO (Either Text YarnBall)
root = do 
    path    <- head <$> getArgs
    content <- TextIO.readFile path
    let expr = parseRoot path content
    textPrint expr
    return expr

compileRoot :: Either Text YarnBall -> IO ()
compileRoot (Left _) = undefined
compileRoot (Right yarnball) = do
    let js = toJS yarnball
    let context = TranspilerContext mempty mempty
    let compilationOutput = transpile context js
    TextIO.putStrLn compilationOutput

main :: IO ()
main = do
    TextIO.putStrLn "\n\n"
    x <- root --expression --putStrLn "todo"
    compileRoot x
