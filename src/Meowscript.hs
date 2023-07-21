{-# LANGUAGE OverloadedStrings #-}

{- This module re-exports the Primitive constructor
 - and core functions. -}

module Meowscript
( ) where

import Meowscript.Core.AST
import Meowscript.API.Console
import Meowscript.Core.RunEvaluator
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Control.StopWatch (stopWatch)

{-
runBasic :: FilePath -> IO ()
runBasic path = do
    (output, time) <- stopWatch $ runMeow (Text.pack path)
    case output of
        (Right x) -> print time >> print x
        (Left x) -> TextIO.putStrLn (snd x)
-}
