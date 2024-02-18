{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Data.Text (Text)
import Mewlix.Compiler.Run
import Mewlix.Compiler.Transpiler (emptyContext)
import Mewlix.Utils.FileIO (readFileT)
import System.Environment (getArgs)
import qualified Data.Text.IO as TextIO

compileFile :: FilePath -> IO Text
compileFile path = do
    contents <- readFileT path >>= \case
        (Left e)    -> undefined
        (Right a)   -> return a
    case compileJS emptyContext path contents of
        (Left e)    -> undefined
        (Right a)   -> return a

main :: IO ()
main = do
    [path] <- getArgs
    compileFile path >>= TextIO.putStrLn
