module Meowscript.Libraries.Utils
( makeIFunc
) where

import Meowscript.Abstract.Meow
import Meowscript.Data.Key (Key)
import qualified Meowscript.Data.Stack as Stack

makeIFunc :: Key -> [Key] -> IFunc -> (Key, MeowPrim)
makeIFunc key arglist f = do
    let params = Stack.fromList arglist
    let arity  = Stack.length params
    let func   = MeowIFunction {
        ifuncName   = key,
        ifuncArity  = arity,
        ifuncParams = params,
        ifunc       = f
    }
    (key, MeowIFunc func)
