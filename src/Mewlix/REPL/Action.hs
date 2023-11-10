module Mewlix.REPL.Action
(
) where

import Mewlix.Abstract.Meow
import Mewlix.Abstract.Prettify
import Mewlix.Interpreter.Exceptions
import Mewlix.Interpreter.API
import Mewlix.REPL.Core
import Mewlix.REPL.Expr

replAction :: LineCommand -> REPL MewlixState
replAction line = case commandAction line of
    Help -> undefined
