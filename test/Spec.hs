{-# LANGUAGE LambdaCase #-}

import Mewlix.Test.Compiler (compileFile, compileTests)
import System.Environment (getArgs)
import qualified Data.Text.IO as TextIO
import Control.Monad ((>=>))

main :: IO ()
main = getArgs >>= \case
    []    -> compileTests
    paths -> mapM_ (compileFile >=> TextIO.putStrLn) paths
