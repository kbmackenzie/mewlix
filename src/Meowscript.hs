{-# LANGUAGE OverloadedStrings #-}

module Meowscript
( runBasic
) where

import Meowscript.Core.AST
import Meowscript.Core.Base
import Meowscript.Core.RunEvaluator
import Meowscript.Parser.RunParser
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Error as MError
import Control.StopWatch (stopWatch)

runBasic :: FilePath -> IO ()
runBasic path = do
    (output, time) <- stopWatch $ runMeow' path
    case output of
        (Right x) -> print time >> print x
        (Left x) -> print x
