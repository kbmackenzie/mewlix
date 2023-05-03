{-# LANGUAGE OverloadedStrings #-}

module Meowscript
( runBasic
) where

import Meowscript.Core.AST
import Meowscript.Core.Base
import Meowscript.Core.Blocks
import Meowscript.Parser.RunParser
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Error as MError
import Control.StopWatch (stopWatch)

runBasic :: FilePath -> IO (Either Text.Text (Prim, EnvStack))
runBasic path = do
    output <- meowParse path
    case output of
        (Right program) -> do
            print program -- Take this off later!
            (tok, time) <- stopWatch $ runEvaluator baseLibrary (runBlock program)
            print time
            return tok
        (Left x) -> (return . Left . Text.pack . MError.errorBundlePretty) x
