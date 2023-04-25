module Meowscript
( runBasic
) where

import Meowscript.Core.AST
import Meowscript.Core.Run
import Meowscript.Core.Base
import Meowscript.Core.Evaluate
import Meowscript.Parser.Statements
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Text.Megaparsec as Mega
import Control.Monad (liftM)

runBasic :: FilePath -> IO (Either Text.Text (Prim, EnvStack))
runBasic path = do
    txt <- TextIO.readFile path
    let output = Mega.parse root path txt
    case output of
        (Right (SAll y)) -> runEvaluator baseLibrary (runStatements y)
        _ -> fail ""
