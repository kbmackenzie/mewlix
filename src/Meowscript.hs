{-# LANGUAGE OverloadedStrings #-}

module Meowscript
( runBasic
) where

import Meowscript.Core.AST
import Meowscript.Core.Base
import Meowscript.Core.Blocks
import Meowscript.Core.Evaluate
import Meowscript.Parser.Statements
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Error as MError
import Control.StopWatch (stopWatch)

runBasic :: FilePath -> IO (Either Text.Text (Prim, EnvStack))
runBasic path = do
    txt <- TextIO.readFile path
    let output = Mega.parse root path txt
    case output of
        (Right (SAll program)) -> do
            print program -- Take this off later!
            (tok, time) <- stopWatch $ runEvaluator baseLibrary (runBlock program)
            print time
            return tok
        (Left x) -> (return . Left . Text.pack . MError.errorBundlePretty) x
        _ -> (return . Left) "Fatal error: Invalid parser output."
